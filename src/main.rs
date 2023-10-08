#![allow(dead_code)]

use inkwell::{context::Context, OptimizationLevel};
use std::fs;

mod ast;
mod codegen;

use ast::*;
use codegen::*;

#[link(name = "swua")]
extern "C" {
    fn print(x: i64) -> i64;
}

fn main() {
    let context = Context::create();
    let mut compiler = Compiler::new(&context, "main");

    use ast_helper::*;
    let program = vec![
        func(
            "add",
            vec![("a".to_string(), Ty::Int), ("b".to_string(), Ty::Int)],
            vec![ret(infix("+", ident("a"), ident("b")))],
        ),
        func(
            "main",
            vec![],
            vec![
                var_init("a", lit(int(10))),
                expr(call(ident("printf"), vec![lit(string("Hello, World!"))])),
                expr(call(ident("print"), vec![ident("a")])),
                ret(call(ident("add"), vec![ident("a"), lit(int(10))])),
            ],
        ),
    ];

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
            .unwrap();

        println!("JIT Return: {}", main.call());
    };
}
