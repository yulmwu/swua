use crate::cli::utils::{
    compile_error, compile_source, display_optimization_level, optimization_level, read_file,
};
use colored::Colorize;
use inkwell::{context::Context, targets::TargetTriple};
use std::{path::PathBuf, process::exit, time::Instant};

pub fn execute(
    input: PathBuf,
    opt_level: u8,
    name: String,
    no_verbose: bool,
    target_triple: TargetTriple,
) {
    if !no_verbose {
        println!(
            "{} {} ({name}) [{}]",
            "Compiling".green().bold(),
            input.display(),
            display_optimization_level(optimization_level(opt_level))
        );
    }

    let now = Instant::now();

    let source_code = read_file(&input);
    let context = Context::create();
    let module = match compile_source(&context, source_code.clone(), &target_triple, &name) {
        Ok(module) => module,
        Err(err) => {
            compile_error(err, &name, input.to_str().unwrap(), source_code);
            exit(1);
        }
    };

    if !no_verbose {
        println!(
            "{} in {} ms",
            "Finished".green().bold(),
            now.elapsed().as_millis()
        );
    }
    let now = Instant::now();

    let engine = module
        .create_jit_execution_engine(optimization_level(opt_level))
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

    if !no_verbose {
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
