use crate::ast::*;
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
    AddressSpace,
};
use std::collections::HashMap;

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(name);

        Compiler {
            context: &context,
            builder,
            module,
            variables: HashMap::new(),
        }
    }

    pub fn compile_module(&mut self, name: String, program: Vec<Statements>) {
        self.module = self.context.create_module(name.as_str());

        // built in functions
        self.module.add_function(
            "printf",
            self.context.i64_type().fn_type(
                vec![BasicMetadataTypeEnum::PointerType(
                    self.context.i8_type().ptr_type(AddressSpace::from(0)),
                )]
                .as_slice(),
                true,
            ),
            Some(Linkage::External),
        );

        self.module.add_function(
            "print",
            self.context.i64_type().fn_type(
                vec![BasicMetadataTypeEnum::IntType(self.context.i64_type())].as_slice(),
                true,
            ),
            Some(Linkage::External),
        );

        for stmt in program {
            match stmt {
                Statements::Declaration(decl) => match decl {
                    Declaration::Variable(var) => self.compile_variable_declaration(var),
                    Declaration::Function(func) => self.compile_function_declaration(func),
                },
                Statements::Return(expr) => {
                    self.compile_expression(expr);
                }
                Statements::Expression(expr) => {
                    self.compile_expression(expr);
                }
            }
        }
    }

    pub fn compile_expression(&mut self, expr: Expression) -> BasicValueEnum<'ctx> {
        match expr {
            Expression::Literal(lit) => self.compile_literal_expression(lit),
            Expression::Identifier(ident) => self.compile_identifier_expression(ident),
            Expression::Call(call) => self.compile_call_expression(call),
            Expression::Infix(infix) => self.compile_infix_expression(infix),
        }
    }

    fn compile_variable_declaration(&mut self, var: VariableDeclaration) {
        let name = var.name;
        let initializer = var.initializer;

        let alloca = self
            .builder
            .build_alloca(self.context.i64_type(), name.as_str());

        if let Some(expr) = initializer {
            let value = self.compile_expression(expr);
            self.builder.build_store(alloca, value);
        }

        self.variables.insert(name, alloca);
    }

    fn compile_function_declaration(&mut self, func: FunctionDeclaration) {
        let name = func.name;
        let parameters = func.parameters;
        let body = func.body;

        let parameters_ty = parameters
            .iter()
            .map(|param| param.1.to_llvm_type_meta(self.context))
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
                .build_alloca(self.context.i64_type(), parameters[i].0.as_str());

            self.builder.build_store(alloca, param);

            self.variables.insert(parameters[i].0.clone(), alloca);
        }

        for stmt in body {
            match stmt {
                Statements::Declaration(decl) => match decl {
                    Declaration::Variable(var) => self.compile_variable_declaration(var),
                    Declaration::Function(func) => self.compile_function_declaration(func),
                },
                Statements::Return(expr) => {
                    let value = self.compile_expression(expr);
                    self.builder.build_return(Some(&value));
                    return;
                }
                Statements::Expression(expr) => {
                    self.compile_expression(expr);
                }
            }
        }

        self.builder.build_return(None);
    }

    fn compile_literal_expression(&mut self, lit: LiteralExpression) -> BasicValueEnum<'ctx> {
        match lit {
            LiteralExpression::Integer(i) => self
                .context
                .i64_type()
                .const_int(i as u64, false)
                .as_basic_value_enum(),
            LiteralExpression::Float(f) => {
                self.context.f64_type().const_float(f).as_basic_value_enum()
            }
            LiteralExpression::String(s) => self
                .builder
                .build_global_string_ptr(s.as_str(), ".str")
                .as_basic_value_enum(),
            LiteralExpression::Boolean(b) => self
                .context
                .bool_type()
                .const_int(b as u64, false)
                .as_basic_value_enum(),
            LiteralExpression::Array(arr, ty) => {
                let mut values: Vec<BasicValueEnum> = Vec::new();

                for val in arr {
                    values.push(self.compile_expression(val));
                }

                let array_ty = ty
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
        }
    }

    fn compile_identifier_expression(
        &mut self,
        ident: IdentifierExpression,
    ) -> BasicValueEnum<'ctx> {
        let name = ident.name;

        let alloca = self.variables.get(&name).unwrap();

        self.builder
            .build_load(self.context.i64_type(), *alloca, name.as_str())
    }

    fn compile_call_expression(&mut self, call: CallExpression) -> BasicValueEnum<'ctx> {
        let callee = call.callee;
        let arguments = call.arguments;

        let function = match *callee {
            Expression::Identifier(ident) => self.module.get_function(ident.name.as_str()).unwrap(),
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
            _ => panic!("Unknown type"),
        }
    }

    fn compile_int_infix_expression(
        &mut self,
        operator: String,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let left = left.into_int_value();
        let right = right.into_int_value();

        match operator.as_str() {
            "+" => self
                .builder
                .build_int_add(left, right, "add")
                .as_basic_value_enum(),
            "-" => self
                .builder
                .build_int_sub(left, right, "sub")
                .as_basic_value_enum(),
            "*" => self
                .builder
                .build_int_mul(left, right, "mul")
                .as_basic_value_enum(),
            "/" => self
                .builder
                .build_int_signed_div(left, right, "div")
                .as_basic_value_enum(),
            "%" => self
                .builder
                .build_int_signed_rem(left, right, "rem")
                .as_basic_value_enum(),
            _ => panic!("Unknown operator"),
        }
    }

    fn compile_float_infix_expression(
        &mut self,
        operator: String,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let left = left.into_float_value();
        let right = right.into_float_value();

        match operator.as_str() {
            "+" => self
                .builder
                .build_float_add(left, right, "add")
                .as_basic_value_enum(),
            "-" => self
                .builder
                .build_float_sub(left, right, "sub")
                .as_basic_value_enum(),
            "*" => self
                .builder
                .build_float_mul(left, right, "mul")
                .as_basic_value_enum(),
            "/" => self
                .builder
                .build_float_div(left, right, "div")
                .as_basic_value_enum(),
            "%" => self
                .builder
                .build_float_rem(left, right, "rem")
                .as_basic_value_enum(),
            _ => panic!("Unknown operator"),
        }
    }

    fn compile_pointer_infix_expression(
        &mut self,
        _operator: String,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let left = left.into_pointer_value();
        let right = right.into_pointer_value();

        println!("{:#?}", left);
        println!("{:#?}", right);

        BasicValueEnum::PointerValue(left)
    }
}
