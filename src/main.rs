use inkwell::{context::Context, OptimizationLevel};
use std::fs;
use swua::codegen::Compiler;

fn main() {
    let context = Context::create();
    let mut compiler = Compiler::new(&context, "main");

    // use ast_helper::*;
    // let program = vec![
    //     func(
    //         "add",
    //         vec![("a".to_string(), Ty::Int), ("b".to_string(), Ty::Int)],
    //         vec![ret(infix("+", ident("a"), ident("b")))],
    //     ),
    //     func(
    //         "main",
    //         vec![],
    //         vec![
    //             var_init("a", lit(int(10))),
    //             expr(call(
    //                 ident("print_str"),
    //                 vec![call(ident("print_str"), vec![lit(string("Hello, World!"))])],
    //             )),
    //             ret(call(ident("add"), vec![ident("a"), lit(int(10))])),
    //         ],
    //     ),
    // ];

    use swua::ast::*;
    let mut program = Program::new();

    let position = Position(0, 0); // TODO

    program.push(Statement::FunctionDeclaration(FunctionDeclaration {
        identifier: Identifier {
            value: "add".to_string(),
            position,
        },
        function: FunctionLiteral {
            parameters: vec![
                Parameter {
                    identifier: Identifier {
                        value: "a".to_string(),
                        position,
                    },
                    ty: Ty {
                        kind: TyKind::Int,
                        position,
                    },
                    position,
                },
                Parameter {
                    identifier: Identifier {
                        value: "b".to_string(),
                        position,
                    },
                    ty: Ty {
                        kind: TyKind::Int,
                        position,
                    },
                    position,
                },
            ],
            body: BlockExpression {
                statements: vec![Statement::ReturnStatement(ReturnStatement {
                    value: Expression::InfixExpression(InfixExpression {
                        operator: InfixOperator::Plus,
                        left: Box::new(Expression::Literal(Literal::Identifier(Identifier {
                            value: "a".to_string(),
                            position,
                        }))),
                        right: Box::new(Expression::Literal(Literal::Identifier(Identifier {
                            value: "b".to_string(),
                            position,
                        }))),
                        position,
                    }),
                    position,
                })],
                position,
            },
            generics: None,
            ret: Ty {
                kind: TyKind::Int,
                position,
            },
            position,
        },
    }));

    program.push(Statement::FunctionDeclaration(FunctionDeclaration {
        identifier: Identifier {
            value: "main".to_string(),
            position,
        },
        function: FunctionLiteral {
            parameters: vec![],
            body: BlockExpression {
                statements: vec![
                    Statement::LetStatement(LetStatement {
                        identifier: Identifier {
                            value: "a".to_string(),
                            position,
                        },
                        value: Some(Expression::Literal(Literal::Int(IntLiteral {
                            value: 10,
                            position,
                        }))),
                        ty: None,
                        is_mutable: false,
                        position,
                    }),
                    Statement::ExpressionStatement(ExpressionStatement {
                        expression: Expression::CallExpression(CallExpression {
                            function: Box::new(Expression::Literal(Literal::Identifier(
                                Identifier {
                                    value: "print_str".to_string(),
                                    position,
                                },
                            ))),
                            arguments: vec![Expression::CallExpression(CallExpression {
                                function: Box::new(Expression::Literal(Literal::Identifier(
                                    Identifier {
                                        value: "print_str".to_string(),
                                        position,
                                    },
                                ))),
                                arguments: vec![Expression::Literal(Literal::String(
                                    StringLiteral {
                                        value: "Hello, World!".to_string(),
                                        position,
                                    },
                                ))],
                                position,
                            })],
                            position,
                        }),
                        position,
                    }),
                    Statement::ReturnStatement(ReturnStatement {
                        value: Expression::CallExpression(CallExpression {
                            function: Box::new(Expression::Literal(Literal::Identifier(
                                Identifier {
                                    value: "add".to_string(),
                                    position,
                                },
                            ))),
                            arguments: vec![
                                Expression::Literal(Literal::Identifier(Identifier {
                                    value: "a".to_string(),
                                    position,
                                })),
                                Expression::Literal(Literal::Int(IntLiteral {
                                    value: 10,
                                    position,
                                })),
                            ],
                            position,
                        }),
                        position,
                    }),
                ],
                position,
            },
            generics: None,
            ret: Ty {
                kind: TyKind::Int,
                position,
            },
            position,
        },
    }));

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
