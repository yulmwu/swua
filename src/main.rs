use inkwell::{context::Context, OptimizationLevel};
use std::fs;
use swua::{codegen::Compiler, parser::Parser, tokenizer::Lexer};

fn main() {
    let context = Context::create();
    let mut compiler = Compiler::new(&context, "main");

    let lexer = Lexer::new(
        r#"
fn add(a: int, b: int) -> int {
    return a + b;
}

fn main() -> int {
    let a = 10;
    print_str(print_str("Hello, World!"));
    return add(a, 10);
}
"#
        .trim(),
    );
    let program = Parser::new(lexer).parse_program().unwrap();

    compiler.compile_module("main".to_string(), program);

    // println!("{}", compiler.module.print_to_string().to_string());
    fs::write(
        "./build/main.ll",
        compiler.module.print_to_string().to_string(),
    )
    .unwrap();

    type JitMainFunc = unsafe extern "C" fn() -> i64;

    unsafe {
        let main = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap()
            .get_function::<JitMainFunc>("main")
            .unwrap();

        println!("JIT Return: {}", main.call());
    };
}
