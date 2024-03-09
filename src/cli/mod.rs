pub mod subcommands;
pub mod utils;

use std::{path::PathBuf, process::exit};

use self::subcommands::{build, repl, run};
use clap::{Parser as ClapParser, Subcommand};
use colored::Colorize;
use guess_host_triple::guess_host_triple;
use inkwell::targets::TargetTriple;

#[derive(ClapParser, Debug)]
#[clap(bin_name = "swua", version = "0.0.0", arg_required_else_help = true)]
pub struct Cli {
    #[clap(subcommand)]
    subcommand: SubCommand,
}

#[derive(Subcommand, Debug)]
pub enum SubCommand {
    #[clap(name = "run", about = "JIT compile and run Swua source code")]
    Run {
        #[clap(short, long)]
        input: PathBuf,
        #[clap(
            short = 'O',
            long,
            help = "Optimization level (0-3, default: 0)",
            default_value_t = 0
        )]
        optimization_level: u8,
        #[clap(
            short,
            long,
            help = "Binary name (default: main)",
            default_value = "main"
        )]
        name: String,
        #[clap(long, help = "Don't print verbose information")]
        no_verbose: bool,
    },
    #[clap(name = "build", about = "Compile Swua source code to native code")]
    Build {
        #[clap(short, long)]
        input: PathBuf,
        #[clap(short, long, help = "Build output directory (default: ./build)")]
        output_dir: Option<PathBuf>,
        #[clap(
            short = 'O',
            long,
            help = "Optimization level (0-3, default: 0)",
            default_value_t = 0
        )]
        optimization_level: u8,
        #[clap(
            short,
            long,
            help = "Binary name (default: main)",
            default_value = "main"
        )]
        name: String,
        #[clap(long, help = "Don't print verbose information")]
        no_verbose: bool,
        #[clap(short, long, help = "Create LLVM IR file")]
        llvm_ir: bool,
        #[clap(short, long, help = "Create ASM file")]
        asm: bool,
        #[clap(short = 'L', long, help = "Link libraries")]
        link: Option<Vec<String>>,
    },
    #[clap(name = "repl", about = "Start Swua REPL")]
    Repl,
}

impl Cli {
    pub fn run() {
        let cli = Cli::parse();

        let triple = guess_host_triple().unwrap_or_else(|| {
            eprintln!("{}", "Error: Unknown target triple".red().bold());
            exit(1);
        });
        let target_triple = TargetTriple::create(triple);

        match cli.subcommand {
            SubCommand::Run {
                input,
                optimization_level: opt_level,
                name,
                no_verbose,
            } => run::execute(input, opt_level, name, no_verbose, target_triple),
            SubCommand::Build {
                input,
                output_dir,
                optimization_level: opt_level,
                name,
                no_verbose,
                llvm_ir,
                asm,
                link,
            } => build::execute(
                input,
                output_dir,
                opt_level,
                name,
                no_verbose,
                llvm_ir,
                asm,
                link,
                target_triple,
            ),
            SubCommand::Repl => repl::execute(),
        }
    }
}
