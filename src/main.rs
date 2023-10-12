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
    // let a: int = 10;
    // print_str(print_str("Hello, World!"));
    let arr: int[] = [int; 1, 2, 3, 4, 5];
    // print_array(arr, 5);
    print(arr[4]);
    // let str: string = "Hello, World!";
    // print_str(str);
    return add(a, 10);
}
"#
        .trim(),
    );
    let program = Parser::new(lexer).parse_program().unwrap();

    compiler.compile_module("main".to_string(), program);

    println!("{}", compiler.module.print_to_string().to_string());
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
            .expect("Failed to find main function");

        println!("JIT Return: {}", main.call());
    };
}
