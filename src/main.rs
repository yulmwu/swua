use clap::{Parser as ClapParser, Subcommand};
use colored::Colorize;
use guess_host_triple::guess_host_triple;
use inkwell::{
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, RelocMode, Target, TargetTriple},
    OptimizationLevel,
};
use std::{
    fs,
    path::{Path, PathBuf},
    process::{exit, Command},
    time::Instant,
};
use swua::{
    codegen::{symbol_table::SymbolTable, CompileError, CompileResult},
    lexer::Lexer,
    parser::{Parser, ParsingError},
};

fn compile<'a>(
    context: &'a Context,
    source_code: String,
    triple: &TargetTriple,
    name: &str,
) -> CompileResult<Module<'a>> {
    let mut lexer = Lexer::new(source_code);
    lexer.tokenize().map_err(ParsingError::from)?;

    let program = Parser::new(lexer.tokens.into_iter())
        .parse_program()
        .map_err(CompileError::from)?;
    // println!("{}", program);

    program.codegen(context, SymbolTable::default(), triple, name)
}

fn compile_error(err: CompileError, name: &str, filename: &str, file_content: String) {
    println!("{}:", "Compilation failed due to".red().bold());

    let lines: Vec<&str> = file_content.split('\n').collect();
    let line = lines[err.span.start.line - 1];

    let spacing = err.span.start.line.to_string().len();
    println!("{}", format!(" {} |", " ".repeat(spacing)).blue());
    println!("{}{line}", format!(" {} |", err.span.start.line).blue());
    println!(
        "{}{}{}",
        format!(" {} |", " ".repeat(spacing)).blue(),
        " ".repeat(err.span.start.column - 1),
        format!("^ Error: {}", err.kind).red().underline()
    );

    println!(" {} {filename}:{} ({name})", "--->".blue(), err.span.start);
}

#[derive(ClapParser, Debug)]
#[clap(bin_name = "swua", version = "0.0.0", arg_required_else_help = true)]
pub struct Cli {
    #[clap(subcommand)]
    pub subcommand: SubCommand,
    #[clap(short, long, help = "Optimization level (0-3, default: 0)")]
    pub optimization_level: Option<u8>,
    #[clap(long, help = "Build output directory (default: ./build)")]
    pub output_dir: Option<PathBuf>,
    #[clap(short, long, help = "Binary name (default: main)")]
    pub name: Option<String>,
    #[clap(long, help = "Don't print verbose information")]
    pub no_verbose: bool,
}

#[derive(Subcommand, Debug)]
pub enum SubCommand {
    #[clap(name = "run", about = "JIT compile and run Swua source code")]
    Run {
        #[clap(short, long)]
        input: PathBuf,
    },
    #[clap(name = "build", about = "Compile Swua source code to native code")]
    Build {
        #[clap(short, long)]
        input: PathBuf,
        #[clap(short, long, help = "Create LLVM IR file")]
        llvm_ir: bool,
        #[clap(short, long, help = "Create ASM file")]
        asm: bool,
    },
}

fn display_optimization_level(level: OptimizationLevel) -> &'static str {
    match level {
        OptimizationLevel::None => "Unoptimized",
        OptimizationLevel::Less => "Less optimized",
        OptimizationLevel::Default => "Default optimized",
        OptimizationLevel::Aggressive => "Aggressively optimized",
    }
}

fn read_file(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!(
            "{}",
            format!("Error: Failed to read file {}: {}", path.display(), err).red()
        );
        exit(1);
    })
}

fn write_file(path: &Path, content: String) {
    fs::write(path, content).unwrap_or_else(|err| {
        eprintln!(
            "{}",
            format!("Error: Failed to write file {}: {}", path.display(), err).red()
        );
        exit(1);
    })
}

fn main() {
    let cli = Cli::parse();
    let optimization_level = match cli.optimization_level.unwrap_or(0) {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        3 => OptimizationLevel::Aggressive,
        _ => {
            eprintln!(
                "{}",
                "Error: Optimization level must be between 0 and 3".red()
            );
            exit(1);
        }
    };
    let name = cli.name.unwrap_or_else(|| "main".to_string());
    let output_dir = cli.output_dir.unwrap_or_else(|| PathBuf::from("./build"));

    let triple = guess_host_triple().unwrap_or_else(|| {
        eprintln!("{}", "Error: Unknown target triple".red().bold());
        exit(1);
    });
    let target_triple = TargetTriple::create(triple);

    match cli.subcommand {
        SubCommand::Run { input } => {
            if !cli.no_verbose {
                println!(
                    "{} {} ({name}) [{}]",
                    "Compiling".green().bold(),
                    input.display(),
                    display_optimization_level(optimization_level)
                );
            }

            let now = Instant::now();

            let source_code = read_file(&input);
            let context = Context::create();
            let module = match compile(&context, source_code.clone(), &target_triple, &name) {
                Ok(module) => module,
                Err(err) => {
                    compile_error(err, &name, input.to_str().unwrap(), source_code);
                    exit(1);
                }
            };

            if !cli.no_verbose {
                println!(
                    "{} in {} ms",
                    "Finished".green().bold(),
                    now.elapsed().as_millis()
                );
            }
            let now = Instant::now();

            let engine = module
                .create_jit_execution_engine(optimization_level)
                .unwrap_or_else(|err| {
                    eprintln!(
                        "{}",
                        format!("Error: Failed to create JIT engine: {}", err).red()
                    );
                    exit(1);
                });

            type JitMainFunction = unsafe extern "C" fn() -> i64;

            let main_return = unsafe {
                engine
                    .get_function::<JitMainFunction>("main")
                    .expect("Failed to find function main")
                    .call()
            };

            if !cli.no_verbose {
                println!(
                    "{} in {} ms, `main` function returned: {}",
                    "Run Finished".green().bold(),
                    now.elapsed().as_millis(),
                    match main_return {
                        0 => "0".green().bold(),
                        ret => ret.to_string().red().bold(),
                    }
                );
            }
        }
        SubCommand::Build {
            input,
            llvm_ir,
            asm,
        } => {
            if !cli.no_verbose {
                println!(
                    "{} {} ({name}) [{}, Target: {}]",
                    "Compiling".green().bold(),
                    input.display(),
                    display_optimization_level(optimization_level),
                    triple
                );
            }

            let now = Instant::now();

            let source_code = read_file(&input);
            let context = Context::create();
            let module = match compile(&context, source_code.clone(), &target_triple, &name) {
                Ok(module) => module,
                Err(err) => {
                    compile_error(err, &name, input.to_str().unwrap(), source_code);
                    exit(1);
                }
            };
            let output = output_dir.join(name);

            if llvm_ir {
                write_file(
                    &output.with_extension("ll"),
                    module.print_to_string().to_string(),
                );
            }

            Target::initialize_all(&Default::default());

            let target = Target::from_triple(&target_triple).unwrap_or_else(|_| {
                eprintln!(
                    "{}",
                    format!("Error: Failed to create target for {}", triple).red()
                );
                exit(1);
            });
            let target_machine = target
                .create_target_machine(
                    &target_triple,
                    "generic",
                    "",
                    optimization_level,
                    RelocMode::Default,
                    CodeModel::Default,
                )
                .unwrap_or_else(|| {
                    eprintln!("{}", "Error: Failed to create target machine".red());
                    exit(1);
                });

            if asm {
                target_machine
                    .write_to_file(
                        &module,
                        FileType::Assembly,
                        Path::new(&output.with_extension("s")),
                    )
                    .unwrap_or_else(|err| {
                        eprintln!(
                            "{}",
                            format!("Error: Failed to write assembly file: {}", err).red()
                        );
                        exit(1);
                    });
            }

            target_machine
                .write_to_file(
                    &module,
                    FileType::Object,
                    Path::new(&output.with_extension("o")),
                )
                .unwrap_or_else(|err| {
                    eprintln!(
                        "{}",
                        format!("Error: Failed to write object file: {}", err).red()
                    );
                    exit(1);
                });

            let command = Command::new("clang")
                .arg("-o")
                .arg(output.clone())
                .arg(output.with_extension("o"))
                .arg("-L")
                .arg(output.parent().unwrap())
                .arg("-l")
                .arg("swua")
                .output()
                .unwrap_or_else(|err| {
                    eprintln!(
                        "{}",
                        format!("Error: Failed to execute clang: {}", err).red()
                    );
                    exit(1);
                });

            let exit_code = command.status.code().unwrap_or_else(|| {
                eprintln!("{}", "Error: clang terminated by signal".red());
                exit(1);
            });
            if exit_code != 0 {
                eprintln!(
                    "{}",
                    command
                        .stderr
                        .iter()
                        .map(|&byte| byte as char)
                        .collect::<String>()
                        .red()
                );
                eprintln!(
                    "{}",
                    format!("Error: clang exited with code {}", exit_code).red()
                );
                exit(exit_code);
            }

            if !cli.no_verbose {
                println!(
                    "{} in {} ms, output: {}",
                    "Build Finished".green().bold(),
                    now.elapsed().as_millis(),
                    output.display()
                );
            }
        }
    }
}
