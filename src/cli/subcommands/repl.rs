use crate::{
    cli::utils::{interpret_error, write_file},
    interpreter::{
        errors::{InterpretError, InterpretResult},
        value::Value,
        Interpreter,
    },
    lexer::Lexer,
    parser::{Parser, ParsingError},
    preprocessor::Preprocessor,
};
use colored::Colorize;
use std::{
    io::{self, Write},
    path::Path,
};

fn read_line() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.to_string()
}

fn interpret(input: String, interpreter: &mut Interpreter) -> InterpretResult<Option<Value>> {
    let mut lexer = Lexer::new(input);
    lexer
        .tokenize()
        .map_err(ParsingError::from)
        .map_err(InterpretError::from)?;

    let mut preprocessor =
        Preprocessor::new_with_defines(lexer.tokens.into_iter(), interpreter.defines.clone());
    let tokens = preprocessor.preprocess().map_err(InterpretError::from)?;
    interpreter.defines = preprocessor.defines;

    let program = Parser::new(tokens.into_iter())
        .parse_program()
        .map_err(InterpretError::from)?;

    interpreter.interpret(program).map_err(InterpretError::from)
}

pub fn execute() {
    let mut interpreter = Interpreter::new();

    let mut is_previous_error = false;
    let mut debug = false;
    let mut command_prefix = String::from("// ");
    let mut input_prefix = String::from(">> ");

    println!("Type '{command_prefix}help' for help, '{command_prefix}quit' to exit the REPL.");

    loop {
        print!(
            "{}",
            if is_previous_error {
                input_prefix.red()
            } else {
                input_prefix.bright_green()
            }
        );
        io::stdout().flush().unwrap();

        let input = read_line();
        let mut inputs: Vec<(bool, String)> = Vec::new();

        if input.starts_with(command_prefix.trim()) {
            let input = input.trim_start_matches(command_prefix.trim()).trim();
            let input = input.split_whitespace().collect::<Vec<&str>>();

            match input[0] {
                "help" | "h" => {
                    println!("* {}", "help or h".blue());
                    println!("\tPrints this help message.");
                    println!();
                    println!("* {}", "debug or dbg".blue());
                    println!("\tToggle debug mode.\n\twhen enabled, the return value of an 'expression' or 'return' statement is printed in detail.");
                    println!();
                    println!("* {}", "history or his".blue());
                    println!("\tPrints execution history.\n\tYou can create a file containing execution history through '{command_prefix}history <File name>'.");
                    println!();
                    println!("* {}", "config or cfg".blue());
                    println!("\tConfigure the REPL.\n\tIf there are no arguments, a description of the config is printed.");
                    println!();
                    println!("* {}", "editor or e".blue());
                    println!("\tOpen the editor. You can exit the editor by typing '{command_prefix}q' and it will execute the code.");
                    println!();
                    println!("* {}", "quit or q".blue());
                    println!("\tExit the REPL.");
                }
                "debug" | "dbg" => {
                    debug = !debug;
                    println!("debug mode: {}", debug);
                }
                "history" | "his" => {
                    if input.len() > 1 {
                        let file_name = input.clone().remove(1);
                        write_file(Path::new(file_name), {
                            let mut content = String::new();
                            for input in inputs.iter() {
                                content.push_str(input.1.as_str());
                                content.push('\n');
                            }
                            content
                        });
                    } else {
                        for (i, input) in inputs.iter().enumerate() {
                            println!(
                                "{}: {}",
                                if input.0 {
                                    i.to_string().green()
                                } else {
                                    i.to_string().red()
                                },
                                input.1
                            );
                        }
                    }
                }
                "config" | "cfg" => {
                    if input.len() > 1 {
                        match input[1] {
                            "command_prefix" => {
                                if input.len() > 2 {
                                    command_prefix = input[2].to_string();
                                    println!("New command prefix: {}", command_prefix);
                                } else {
                                    println!("Command prefix: {}", command_prefix);
                                }
                            }
                            "input_prefix" => {
                                if input.len() > 2 {
                                    input_prefix = input[2].to_string();
                                } else {
                                    println!("input_prefix: {}", input_prefix);
                                }
                            }
                            _ => {
                                println!("unknown config: {}", input[1]);
                            }
                        }
                    } else {
                        println!("* {}", "command_prefix".blue());
                        println!("\tThe prefix for commands.");
                        println!();
                        println!("* {}", "input_prefix".blue());
                        println!("\tThe prefix for input.");
                    }
                }
                "editor" | "e" => {
                    println!("Entered editor mode. Type '{command_prefix}q' to exit the editor and execute the code.");
                    println!("To exit or cancel, press Enter twice.");

                    let mut previous = "Dummy".to_string();
                    let mut code = String::new();
                    let mut line = 1;
                    loop {
                        print!("{} ", format!("{line}").on_bright_white().black());
                        io::stdout().flush().unwrap();

                        let input = read_line();

                        if previous.trim().is_empty() && input.trim().is_empty() {
                            print!(
                                "{} ",
                                "Would you like to exit(`e`) or clear(`c`) the input? (E/C): "
                                    .on_white()
                                    .black()
                            );
                            io::stdout().flush().unwrap();

                            match read_line().trim() {
                                "e" | "E" => break,
                                "c" | "C" => {
                                    code.clear();
                                    line = 1;
                                    continue;
                                }
                                _ => continue,
                            }
                        }

                        code.push_str(input.as_str());
                        code.push('\n');

                        line += 1;
                        previous = input;
                    }

                    match interpret(code, &mut interpreter) {
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
                    }
                }
                "quit" | "q" => {
                    break;
                }
                _ => {}
            }
        } else {
            match interpret(input.clone(), &mut interpreter) {
                Ok(value) => {
                    inputs.push((true, input.clone()));

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
                    inputs.push((false, input.clone()));

                    is_previous_error = true;
                    interpret_error(err)
                }
            }
        }
    }
}
