use clap::{Parser as ClapParser, Subcommand};
use colored::Colorize;
use inkwell::{context::Context, module::Module, OptimizationLevel};
use std::{fs, path::PathBuf, time::Instant};
use swua::{
    codegen::{CompileError, CompileResult},
    parser::Parser,
    tokenizer::Lexer,
    SymbolTable,
};

fn compile<'a>(context: &'a Context, source_code: &str) -> CompileResult<Module<'a>> {
    let program = Parser::new(Lexer::new(source_code))
        .parse_program()
        .map_err(|err| CompileError::from(err[0].clone()))?;
    // println!("{}", program);
    program.codegen(context, SymbolTable::default())
}

fn compile_error(err: CompileError, name: &str, filename: &str, file_content: String) {
    println!("{}:", "Compilation failed due to".red().bold());

    let lines: Vec<&str> = file_content.split('\n').collect();
    let line = lines[err.position.line - 1];

    let spacing = err.position.line.to_string().len();
    println!("{}", format!(" {} |", " ".repeat(spacing)).blue());
    println!("{}{line}", format!(" {} |", err.position.line).blue());
    println!(
        "{}{}{}",
        format!(" {} |", " ".repeat(spacing)).blue(),
        " ".repeat(err.position.column - 1),
        format!("^ Error: {}", err.kind).red().underline()
    );

    println!(" {} {filename}:{} ({name})", "--->".blue(), err.position);
}

#[derive(ClapParser, Debug)]
#[clap(bin_name = "swua", version = "0.0.0", arg_required_else_help = true)]
pub struct Cli {
    #[clap(subcommand)]
    pub subcommand: SubCommand,
    #[clap(short, long, help = "Print LLVM IR")]
    pub llvm_ir: bool,
    #[clap(short, long, help = "JIT Optimization level (0-3)")]
    pub optimization_level: Option<u8>,
    #[clap(short, long, help = "LLVM Module name")]
    pub name: Option<String>,
    #[clap(long, help = "Don't print verbose information")]
    pub no_verbose: bool,
}

#[derive(Subcommand, Debug)]
pub enum SubCommand {
    #[clap(name = "compile", about = "Compile Swua source code")]
    Compile {
        #[clap(short, long)]
        input: PathBuf,
        #[clap(short, long)]
        output: PathBuf,
    },
    #[clap(name = "run", about = "JIT compile and run Swua source code")]
    Run {
        #[clap(short, long)]
        input: PathBuf,
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
            return;
        }
    };
    let llvm_module_name = cli.name.unwrap_or_else(|| "main".to_string());

    match cli.subcommand {
        SubCommand::Compile { input, output } => {
            if !cli.no_verbose {
                println!(
                    "{} {} [{}]",
                    "Compiling".green().bold(),
                    input.display(),
                    display_optimization_level(optimization_level)
                );
            }

            let source_code = fs::read_to_string(input.clone()).unwrap();
            let context = Context::create();
            let module = match compile(&context, &source_code) {
                Ok(module) => module,
                Err(err) => {
                    compile_error(err, &llvm_module_name, input.to_str().unwrap(), source_code);
                    return;
                }
            };

            if cli.llvm_ir {
                println!("{}", module.print_to_string().to_string());
            }

            fs::write(output.clone(), module.print_to_string().to_string()).unwrap();

            if !cli.no_verbose {
                println!(
                    "{}: {}",
                    "Compile Finished".green().bold(),
                    output.display()
                );
            }
        }
        SubCommand::Run { input } => {
            if !cli.no_verbose {
                println!(
                    "{} {} [{}]",
                    "Compiling".green().bold(),
                    input.display(),
                    display_optimization_level(optimization_level)
                );
            }

            let now = Instant::now();

            let source_code = fs::read_to_string(input.clone()).unwrap();
            let context = Context::create();
            let module = match compile(&context, &source_code) {
                Ok(module) => module,
                Err(err) => {
                    compile_error(err, &llvm_module_name, input.to_str().unwrap(), source_code);
                    return;
                }
            };

            if !cli.no_verbose {
                println!(
                    "{} in {} ms",
                    "  Compile Finished".green().bold(),
                    now.elapsed().as_millis()
                );
            }
            let now = Instant::now();

            if cli.llvm_ir {
                eprintln!("{}", module.print_to_string().to_string());
            }

            let engine = module
                .create_jit_execution_engine(optimization_level)
                .unwrap();

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
    }
}
