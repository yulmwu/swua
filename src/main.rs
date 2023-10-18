use inkwell::{context::Context, OptimizationLevel};
use std::fs;
use swua::{codegen::Compiler, parser::Parser, tokenizer::Lexer};

fn main() {
    let context = Context::create();
    let mut compiler = Compiler::new(&context, "main");

    let lexer = Lexer::new(
        r#"
// struct Foo {
//     a: int,
//     b: int
// };

extern fn print(int) -> int;
extern fn print_str(string) -> int;
extern fn print_array(int, int) -> int;
// extern fn printf(string) -> int;
// extern fn print_struct(Foo) -> int;

fn main() -> int {
    // let foo: Foo = struct Foo { a: 1, b: 2 };
    // print_struct(foo);
    // print(foo[a]);

    // let x: int[] = [int; 1, 2, 3, 4, 5];
    // print_array(x, 5);
    // print(x[1]);

    // let a: int = 1;
    // print(a);

    // let x: string = "Hello World!";
    // print_str(x);

    // let y: string[] = [string; "Hello", "World!"];
    // print_str(y[0]);
    // print_str(y[1]);

    return 0;
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
