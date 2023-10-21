use clap::{Parser, Subcommand};
use inkwell::{context::Context, OptimizationLevel};
use std::{fs, path::PathBuf};
use swua::codegen::{error::CompileError, Compiler};

fn compile_error(err: CompileError, filename: &str) {
    eprintln!(
        "Compile error at {}:{}: {}",
        filename, err.position, err.kind
    );
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

fn main() {
    let cli = Cli::parse();

    match cli.subcommand {
        SubCommand::Compile { input, output } => {
            let context = Context::create();
            let module = match Compiler::new(&context, "main")
                .compile(&fs::read_to_string(input.clone()).unwrap())
            {
                Ok(module) => module,
                Err(err) => {
                    compile_error(err, input.to_str().unwrap());
                    return;
                }
            };

            if cli.llvm_ir {
                println!("{}", module.print_to_string().to_string());
            }

            fs::write(output, module.print_to_string().to_string()).unwrap();
        }
        SubCommand::Run { input } => {
            let context = Context::create();
            let module = match Compiler::new(&context, "main")
                .compile(&fs::read_to_string(input.clone()).unwrap())
            {
                Ok(module) => module,
                Err(err) => {
                    compile_error(err, input.to_str().unwrap());
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
