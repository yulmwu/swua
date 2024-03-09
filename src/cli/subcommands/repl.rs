use colored::Colorize;

use crate::{
    cli::utils::interpret_error,
    interpreter::{
        errors::{InterpretError, InterpretResult},
        value::Value,
        Interpreter,
    },
    lexer::Lexer,
    parser::{Parser, ParsingError},
};
use std::io::{self, Write};

fn read_line() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

fn interpret(input: String, interpreter: &mut Interpreter) -> InterpretResult<Option<Value>> {
    let mut lexer = Lexer::new(input);
    lexer
        .tokenize()
        .map_err(ParsingError::from)
        .map_err(InterpretError::from)?;

    let program = Parser::new(lexer.tokens.into_iter())
        .parse_program()
        .map_err(InterpretError::from)?;

    interpreter.interpret(program).map_err(InterpretError::from)
}

pub fn execute() {
    println!("type '// help' for help");

    let mut interpreter = Interpreter::new();

    let mut is_previous_error = false;
    let mut debug = false;

    loop {
        print!(
            "{} ",
            if is_previous_error {
                ">>".red()
            } else {
                ">>".green()
            }
        );
        io::stdout().flush().unwrap();

        let input = read_line();

        match input.as_str() {
            "// help" => {
                println!("// {} - display this help message.", "help".blue());
                println!("// {} - toggle debug mode. when enabled, the return value of an 'expression' or 'return' statement is printed in detail.", "debug".blue());
                println!("// {} - exit the REPL.", "quit".red());
            }
            "// debug" => {
                debug = !debug;
                println!("debug mode: {}", debug);
            }
            "// quit" => {
                break;
            }
            _ => match interpret(input.clone(), &mut interpreter) {
                Ok(value) => {
                    is_previous_error = false;
                    if let Some(value) = value {
                        if debug {
                            println!("{value:?}");
                        } else {
                            println!("{value}");
                        }
                    }
                }
                Err(err) => {
                    is_previous_error = true;
                    interpret_error(err)
                }
            },
        }
    }
}
