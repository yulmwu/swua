use crate::{
    codegen::{symbol_table::SymbolTable, CompileError},
    lexer::Lexer,
    parser::Parser,
    preprocessor::Preprocessor,
};
use colored::Colorize;
use inkwell::{context::Context, module::Module, targets::TargetTriple, OptimizationLevel};
use std::{fs, path::Path, process::exit};

pub fn display_optimization_level(level: OptimizationLevel) -> &'static str {
    match level {
        OptimizationLevel::None => "Unoptimized",
        OptimizationLevel::Less => "Less optimized",
        OptimizationLevel::Default => "Default optimized",
        OptimizationLevel::Aggressive => "Aggressively optimized",
    }
}

pub fn read_file(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!(
            "{}",
            format!("Error: Failed to read file {}: {}", path.display(), err).red()
        );
        exit(1);
    })
}

pub fn write_file(path: &Path, content: String) {
    fs::write(path, content).unwrap_or_else(|err| {
        eprintln!(
            "{}",
            format!("Error: Failed to write file {}: {}", path.display(), err).red()
        );
        exit(1);
    })
}

#[inline]
pub fn optimization_level(level: u8) -> OptimizationLevel {
    match level {
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
    }
}

pub fn compile_source<'a>(
    context: &'a Context,
    source_code: String,
    triple: &TargetTriple,
    name: &str,
) -> Result<Module<'a>, CompileError> {
    let mut lexer = Lexer::new(source_code);
    lexer.tokenize().map_err(CompileError::from)?;

    let mut preprocessor = Preprocessor::new(lexer.tokens.into_iter());
    let tokens = preprocessor.preprocess().map_err(CompileError::from)?;

    let program = Parser::new(tokens.into_iter())
        .parse_program()
        .map_err(CompileError::from)?;
    // println!("{}", program);

    program.codegen(context, SymbolTable::default(), triple, name)
}

pub fn compile_error(error: CompileError, name: &str, filename: &str, file_content: String) {
    println!("{}:", "Compilation failed due to".red().bold());

    let lines: Vec<&str> = file_content.split('\n').collect();
    let line = lines[error.span.start.line - 1];
    let spacing = error.span.start.line.to_string().len();

    println!("{}", format!(" {} |", " ".repeat(spacing)).blue());
    println!("{}{line}", format!(" {} |", error.span.start.line).blue());
    println!(
        "{}{}{}",
        format!(" {} |", " ".repeat(spacing)).blue(),
        " ".repeat(error.span.start.column - 1),
        format!("^ Error: {}", error.kind).red().underline()
    );

    if let Some(help) = error.help {
        println!(
            "{} {}: {help}",
            format!(" {} -", " ".repeat(spacing)).blue(),
            "help".green()
        );
    }

    println!(
        " {} {filename}:{} ({name})",
        "--->".blue(),
        error.span.start
    );
}
