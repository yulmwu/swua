use inkwell::{context::Context, OptimizationLevel};
use std::fs;
use swua::{codegen::Compiler, parser::Parser, tokenizer::Lexer};

fn main() {
    let context = Context::create();
    let mut compiler = Compiler::new(&context, "main");

    let lexer = Lexer::new(
        r#"
extern fn print(int) -> int;
extern fn print_str(string) -> int;
extern fn print_array(int, int) -> int;
// extern fn printf(string) -> int;
// extern fn print_struct(Foo, int) -> int;

fn main() -> int {
    let x = ["Hello", "World!"];
    let index_0 = x[0];
    print_str(x[0]);
    print_str(x[1]);
    print_str(index_0);

    print_array([1, 2, 3], 3);
    print([1, 2, 3][2]);

    return 0;
}
"#
        .trim(),
    );
    let program = Parser::new(lexer).parse_program().unwrap();

    compiler
        .compile_module("main".to_string(), program)
        .unwrap();

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
