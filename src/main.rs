use clap::{Parser, Subcommand};
use colored::Colorize;
use inkwell::{context::Context, OptimizationLevel};
use std::{fs, path::PathBuf};
use swua::codegen::{error::CompileError, Compiler};

fn compile_error(err: CompileError, name: &str, filename: &str, file_content: String) {
    println!("{}:", "Compilation failed due to".red().bold());

    let lines: Vec<&str> = file_content.split('\n').collect();
    let line = lines[err.position.0 - 1];

    let spacing = err.position.0.to_string().len();
    println!("{}", format!(" {} |", " ".repeat(spacing)).blue());
    println!("{}{line}", format!(" {} |", err.position.0).blue());
    println!(
        "{}{}{}",
        format!(" {} |", " ".repeat(spacing)).blue(),
        " ".repeat(err.position.1 - 1),
        format!("^ Error: {}", err.kind).red().underline()
    );

    println!(" {} {filename}:{} ({name})", "--->".blue(), err.position);
}

#[derive(Parser, Debug)]
#[clap(bin_name = "swua", version = "0.0.0", arg_required_else_help = true)]
pub struct Cli {
    #[clap(subcommand)]
    pub subcommand: SubCommand,
    #[clap(short, long, help = "Print LLVM IR")]
    pub llvm_ir: bool,
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
    #[clap(name = "run", about = "Jit compile and run Swua source code")]
    Run {
        #[clap(short, long)]
        input: PathBuf,
    },
}

const NAME: &str = "main"; // TODO

fn main() {
    let cli = Cli::parse();

    match cli.subcommand {
        SubCommand::Compile { input, output } => {
            let source_code = fs::read_to_string(input.clone()).unwrap();
            let context = Context::create();
            let module = match Compiler::new(&context, NAME).compile(&source_code) {
                Ok(module) => module,
                Err(err) => {
                    compile_error(err, NAME, input.to_str().unwrap(), source_code);
                    return;
                }
            };

            if cli.llvm_ir {
                println!("{}", module.print_to_string().to_string());
            }

            fs::write(output.clone(), module.print_to_string().to_string()).unwrap();

            println!(
                "{}: {}",
                "Compile Finished".green().bold(),
                output.display()
            );
        }
        SubCommand::Run { input } => {
            let source_code = fs::read_to_string(input.clone()).unwrap();
            let context = Context::create();
            let module = match Compiler::new(&context, NAME).compile(&source_code) {
                Ok(module) => module,
                Err(err) => {
                    compile_error(err, NAME, input.to_str().unwrap(), source_code);
                    return;
                }
            };

            if cli.llvm_ir {
                eprintln!("{}", module.print_to_string().to_string());
            }

            let engine = module
                .create_jit_execution_engine(OptimizationLevel::None)
                .unwrap();

            type JitMainFunction = unsafe extern "C" fn() -> i64;

            unsafe {
                engine
                    .get_function::<JitMainFunction>("main")
                    .expect("Failed to find function main")
                    .call()
            };
        }
    }
}
