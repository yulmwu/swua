use colored::Colorize;
use inkwell::{
    context::Context,
    targets::{CodeModel, FileType, RelocMode, Target, TargetTriple},
};
use std::{
    path::{Path, PathBuf},
    process::{exit, Command},
    time::Instant,
};

use crate::cli::utils::{
    compile_error, compile_source, display_optimization_level, optimization_level, read_file,
    write_file,
};

#[allow(clippy::too_many_arguments)]
pub fn execute(
    input: PathBuf,
    output_dir: Option<PathBuf>,
    opt_level: u8,
    name: String,
    no_verbose: bool,
    llvm_ir: bool,
    asm: bool,
    link: Option<Vec<String>>,
    target_triple: TargetTriple,
) {
    if !no_verbose {
        println!(
            "{} {} ({name}) [{}, Target: {}]",
            "Compiling".green().bold(),
            input.display(),
            display_optimization_level(optimization_level(opt_level)),
            target_triple
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
    let output = output_dir
        .unwrap_or_else(|| PathBuf::from("./build"))
        .join(&name);

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
            format!("Error: Failed to create target for {}", target_triple).red()
        );
        exit(1);
    });
    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            optimization_level(opt_level),
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

    let mut command = Command::new("clang");
    command
        .arg("-o")
        .arg(output.clone())
        .arg(output.with_extension("o"))
        .arg("-L")
        .arg(output.parent().unwrap())
        .arg("-l")
        .arg("swua");

    if let Some(link) = link {
        for lib in link {
            command.arg("-l").arg(lib);
        }
    }

    let command = command.output().unwrap_or_else(|err| {
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

    if !no_verbose {
        println!(
            "{} in {} ms, output: {}",
            "Build Finished".green().bold(),
            now.elapsed().as_millis(),
            output.display()
        );
    }
}
