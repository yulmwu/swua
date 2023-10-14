use crate::ast::*;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
};
use std::collections::HashMap;

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,

    variables: HashMap<String, (PointerValue<'ctx>, TyKind)>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(name);

        Compiler {
            context,
            builder,
            module,
            variables: HashMap::new(),
        }
    }

    pub fn compile_module(&mut self, name: String, program: Program) {
        self.module = self.context.create_module(name.as_str());

        for stmt in program {
            self.compile_statement(stmt);
        }
    }

    pub fn compile_statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::LetStatement(stmt) => self.compile_let_statement(stmt),
            Statement::FunctionDeclaration(func) => self.compile_function_declaration(func),
            Statement::ExternFunctionDeclaration(func) => {
                self.compile_external_function_declaration(func)
            }
            Statement::StructStatement(stmt) => self.compile_struct_statement(stmt),
            Statement::ExpressionStatement(expr) => {
                self.compile_expression(expr.expression);
            }
            _ => unimplemented!(),
        }
    }

    pub fn compile_expression(&mut self, expr: Expression) -> BasicValueEnum<'ctx> {
        match expr {
            Expression::Literal(lit) => self.compile_literal_expression(lit),
            Expression::CallExpression(call) => self.compile_call_expression(call),
            Expression::InfixExpression(infix) => self.compile_infix_expression(infix),
            Expression::IndexExpression(index) => self.compile_index_expression(index),
            _ => unimplemented!(),
        }
    }

    fn compile_let_statement(&mut self, stmt: LetStatement) {
        let name = stmt.identifier.value;
        let initializer = stmt.value;
        let ty = stmt.ty.expect("TODO").kind;

        let alloca = self
            .builder
            .build_alloca(self.context.i64_type(), name.as_str());

        if let Some(expr) = initializer {
            let value = self.compile_expression(expr);
            self.builder.build_store(alloca, value);
        }

        self.variables.insert(name, (alloca, ty));
    }

    fn compile_function_declaration(&mut self, func: FunctionDeclaration) {
        let name = func.identifier.value;
        let parameters = func.function.parameters;
        let body = func.function.body;

        let parameters_ty = parameters
            .iter()
            .map(|param| param.ty.kind.to_llvm_type_meta(self.context))
            .collect::<Vec<_>>();
        let function_type = self
            .context
            .i64_type()
            .fn_type(parameters_ty.as_slice(), false);

        let function = self.module.add_function(name.as_str(), function_type, None);

        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        for (i, param) in function.get_param_iter().enumerate() {
            let alloca = self
                .builder
                .build_alloca(self.context.i64_type(), &parameters[i].identifier.value);

            self.builder.build_store(alloca, param);

            self.variables.insert(
                parameters[i].identifier.value.clone(),
                (alloca, TyKind::from(param.get_type())),
            );
        }

        for stmt in body.statements {
            match stmt {
                Statement::LetStatement(stmt) => self.compile_let_statement(stmt),
                Statement::FunctionDeclaration(func) => self.compile_function_declaration(func),
                Statement::ReturnStatement(expr) => {
                    let value = self.compile_expression(expr.value);
                    self.builder.build_return(Some(&value));

                    break;
                }
                Statement::StructStatement(stmt) => self.compile_struct_statement(stmt),
                Statement::ExpressionStatement(expr) => {
                    self.compile_expression(expr.expression);
                }
                _ => unimplemented!(),
            }
        }
    }

    fn compile_external_function_declaration(&mut self, func: ExternFunctionDeclaration) {
        let name = func.identifier.value;
        let parameters = func.parameters;
        let ret = func.ret.kind;

        self.module.add_function(
            name.as_str(),
            ret.to_llvm_type(self.context).fn_type(
                parameters
                    .iter()
                    .map(|param| param.kind.to_llvm_type_meta(self.context))
                    .collect::<Vec<_>>()
                    .as_slice(),
                false,
            ),
            None,
        );
    }

    fn compile_literal_expression(&mut self, lit: Literal) -> BasicValueEnum<'ctx> {
        match lit {
            Literal::Identifier(ident) => self.compile_identifier_expression(ident),
            Literal::Int(i) => self
                .context
                .i64_type()
                .const_int(i.value as u64, false)
                .as_basic_value_enum(),
            Literal::Float(f) => self
                .context
                .f64_type()
                .const_float(f.value)
                .as_basic_value_enum(),
            Literal::String(s) => self
                .builder
                .build_global_string_ptr(s.value.as_str(), ".str")
                .as_basic_value_enum(),
            Literal::Boolean(b) => self
                .context
                .bool_type()
                .const_int(b.value as u64, false)
                .as_basic_value_enum(),
            Literal::Array(arr) => {
                let mut values: Vec<BasicValueEnum> = Vec::new();

                for val in arr.elements {
                    values.push(self.compile_expression(val));
                }

                let array_ty = arr
                    .ty
                    .kind
                    .to_llvm_type(self.context)
                    .array_type(values.len() as u32);
                let ptr = self
                    .builder
                    .build_alloca(array_ty, "array")
                    .as_basic_value_enum()
                    .into_pointer_value();

                for (i, val) in values.iter().enumerate() {
                    let index = self.context.i64_type().const_int(i as u64, false);

                    let ptr = unsafe {
                        self.builder.build_in_bounds_gep(
                            self.context.i64_type(),
                            ptr,
                            &[index],
                            "ptr",
                        )
                    };

                    self.builder.build_store(ptr, *val);
                }

                ptr.as_basic_value_enum()
            }
            // Literal::ArrayHeap(arr) => {
            //     /*
            //     | len | capacity | ptr |
            //     */

            //     let struct_ty = self.module.get_struct_type("Array").unwrap();
            // }
            Literal::Struct(struct_lit) => {
                let name = struct_lit.identifier.value;
                let struct_ty = self.module.get_struct_type(name.as_str()).unwrap();

                let mut values: Vec<BasicValueEnum> = Vec::new();

                for val in struct_lit.fields {
                    values.push(self.compile_expression(val.1));
                }

                let ptr = self
                    .builder
                    .build_alloca(struct_ty, "struct")
                    .as_basic_value_enum()
                    .into_pointer_value();

                for (i, val) in values.iter().enumerate() {
                    let index = self.context.i64_type().const_int(i as u64, false);

                    let ptr = unsafe {
                        self.builder.build_in_bounds_gep(
                            self.context.i64_type(),
                            ptr,
                            &[index],
                            "ptr",
                        )
                    };

                    self.builder.build_store(ptr, *val);
                }

                ptr.as_basic_value_enum()
            }
            _ => unimplemented!(),
        }
    }

    fn compile_identifier_expression(&mut self, ident: Identifier) -> BasicValueEnum<'ctx> {
        let name = ident.value;

        let (alloca, ty) = self.variables.get(&name).unwrap();
        let ty = ty.to_llvm_type(self.context);

        self.builder.build_load(ty, *alloca, name.as_str())
    }

    fn compile_call_expression(&mut self, call: CallExpression) -> BasicValueEnum<'ctx> {
        let callee = call.function;
        let arguments = call.arguments;

        let function = match *callee {
            Expression::Literal(Literal::Identifier(ident)) => {
                self.module.get_function(ident.value.as_str()).unwrap()
            }
            _ => panic!("Expected identifier expression"),
        };

        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();

        for arg in arguments {
            args.push(self.compile_expression(arg).into());
        }

        self.builder
            .build_call(function, args.as_slice(), "call")
            .try_as_basic_value()
            .left()
            .unwrap()
    }

    fn compile_infix_expression(&mut self, infix: InfixExpression) -> BasicValueEnum<'ctx> {
        let operator = infix.operator;
        let left = infix.left;
        let right = infix.right;

        let left = self.compile_expression(*left);
        let right = self.compile_expression(*right);

        match (left.get_type(), right.get_type()) {
            (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                self.compile_int_infix_expression(operator, left, right)
            }
            (BasicTypeEnum::FloatType(_), BasicTypeEnum::FloatType(_)) => {
                self.compile_float_infix_expression(operator, left, right)
            }
            (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(_)) => {
                self.compile_pointer_infix_expression(operator, left, right)
            }
            // foo.a (getelementptr)
            (BasicTypeEnum::StructType(_), BasicTypeEnum::PointerType(_)) => {
                // let left = left.into_struct_value();
                let right = right.into_pointer_value();

                let ptr = unsafe {
                    self.builder.build_gep(
                        self.context.i64_type(),
                        right,
                        &[left.into_int_value()],
                        "ptr",
                    )
                };

                self.builder
                    .build_load(self.context.i64_type(), ptr, "load")
            }
            _ => panic!("Unknown type: {:?} {:?}", left.get_type(), right.get_type()),
        }
    }

    fn compile_int_infix_expression(
        &mut self,
        operator: InfixOperator,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let left = left.into_int_value();
        let right = right.into_int_value();

        match operator {
            InfixOperator::Plus => self
                .builder
                .build_int_add(left, right, "add")
                .as_basic_value_enum(),
            InfixOperator::Minus => self
                .builder
                .build_int_sub(left, right, "sub")
                .as_basic_value_enum(),
            InfixOperator::Asterisk => self
                .builder
                .build_int_mul(left, right, "mul")
                .as_basic_value_enum(),
            InfixOperator::Slash => self
                .builder
                .build_int_signed_div(left, right, "div")
                .as_basic_value_enum(),
            InfixOperator::Percent => self
                .builder
                .build_int_signed_rem(left, right, "rem")
                .as_basic_value_enum(),
            _ => panic!("Unknown operator"),
        }
    }

    fn compile_float_infix_expression(
        &mut self,
        operator: InfixOperator,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let left = left.into_float_value();
        let right = right.into_float_value();

        match operator {
            InfixOperator::Plus => self
                .builder
                .build_float_add(left, right, "add")
                .as_basic_value_enum(),
            InfixOperator::Minus => self
                .builder
                .build_float_sub(left, right, "sub")
                .as_basic_value_enum(),
            InfixOperator::Asterisk => self
                .builder
                .build_float_mul(left, right, "mul")
                .as_basic_value_enum(),
            InfixOperator::Slash => self
                .builder
                .build_float_div(left, right, "div")
                .as_basic_value_enum(),
            InfixOperator::Percent => self
                .builder
                .build_float_rem(left, right, "rem")
                .as_basic_value_enum(),
            _ => panic!("Unknown operator"),
        }
    }

    fn compile_pointer_infix_expression(
        &mut self,
        _operator: InfixOperator,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let left = left.into_pointer_value();
        let right = right.into_pointer_value();

        let deref = self
            .builder
            .build_load(self.context.i64_type(), left, "deref");
        println!("{:#?}", deref);

        println!("{:#?}", left);
        println!("{:#?}", right);

        BasicValueEnum::PointerValue(left)
    }

    fn compile_index_expression(&mut self, index: IndexExpression) -> BasicValueEnum<'ctx> {
        let left = self.compile_expression(*index.left);
        println!("{:#?}", left);
        let index = self.compile_expression(*index.index);

        let left = left.into_pointer_value();

        let ptr = unsafe {
            self.builder.build_gep(
                self.context.i64_type(),
                left,
                &[index.into_int_value()],
                "ptr",
            )
        };

        self.builder
            .build_load(self.context.i64_type(), ptr, "load")
    }

    fn compile_struct_statement(&mut self, stmt: StructStatement) {
        let name = stmt.identifier.value;
        let fields = stmt.fields;

        let mut fields_ty = Vec::new();

        for field in fields {
            fields_ty.push(field.ty.kind.to_llvm_type(self.context));
        }

        let struct_ty = self.context.opaque_struct_type(name.as_str());

        struct_ty.set_body(fields_ty.as_slice(), false);

        self.module.add_global(struct_ty, None, name.as_str());
    }
}
