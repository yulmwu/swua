use crate::ast::*;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{self, BasicType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct SymbolTable<'a> {
    pub variables: HashMap<String, (PointerValue<'a>, TyKind)>,
    pub structs: HashMap<String, (types::StructType<'a>, StructType)>,
    pub struct_fields: HashMap<PointerValue<'a>, TyKind>,
    pub functions: HashMap<String, (types::FunctionType<'a>, FunctionType)>,
}

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,

    symbol_table: SymbolTable<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(name);

        Compiler {
            context,
            builder,
            module,
            symbol_table: SymbolTable::default(),
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

    pub fn compile_expression(&mut self, expr: Expression) -> (BasicValueEnum<'ctx>, TyKind) {
        let (value, ty) = match expr {
            Expression::Literal(lit) => self.compile_literal_expression(lit),
            Expression::CallExpression(call) => self.compile_call_expression(call),
            Expression::InfixExpression(infix) => self.compile_infix_expression(infix),
            Expression::IndexExpression(index) => self.compile_index_expression(index),
            _ => unimplemented!(),
        };
        (value, ty.analyzed(self.context, self.symbol_table.clone()))
    }

    fn compile_let_statement(&mut self, stmt: LetStatement) {
        let name = stmt.identifier.value;
        let initializer = stmt.value;
        let ty = stmt.ty.expect("TODO").kind;

        let alloca = self
            .builder
            .build_alloca(self.context.i64_type(), name.as_str());

        if let Some(expr) = initializer {
            let value = self.compile_expression(expr).0;
            self.builder.build_store(alloca, value);
        }

        self.symbol_table.variables.insert(name, (alloca, ty));
    }

    fn compile_function_declaration(&mut self, func: FunctionDeclaration) {
        let FunctionDeclaration {
            identifier: Identifier { value: name, .. },
            function:
                FunctionLiteral {
                    parameters,
                    body,
                    generics,
                    ret,
                    position,
                },
            ..
        } = func;

        let parameters_ty = parameters
            .iter()
            .map(|param| {
                param
                    .ty
                    .kind
                    .to_llvm_type(self.context, self.symbol_table.clone())
                    .into()
            })
            .collect::<Vec<_>>();
        let function_type = self
            .context
            .i64_type()
            .fn_type(parameters_ty.as_slice(), false);

        let function = self.module.add_function(name.as_str(), function_type, None);

        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        self.symbol_table.functions.insert(
            name.clone(),
            (
                function_type,
                FunctionType {
                    generics,
                    parameters: parameters.iter().map(|param| param.ty.clone()).collect(),
                    ret: Box::new(ret),
                    position,
                },
            ),
        );

        for (i, param) in function.get_param_iter().enumerate() {
            let alloca = self
                .builder
                .build_alloca(self.context.i64_type(), &parameters[i].identifier.value);

            self.builder.build_store(alloca, param);

            self.symbol_table.variables.insert(
                parameters[i].identifier.value.clone(),
                (alloca, parameters[i].ty.kind.clone()),
            );
        }

        for stmt in body.statements {
            match stmt {
                Statement::LetStatement(stmt) => self.compile_let_statement(stmt),
                Statement::FunctionDeclaration(func) => self.compile_function_declaration(func),
                Statement::ReturnStatement(expr) => {
                    let value = self.compile_expression(expr.value).0;
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
        let ExternFunctionDeclaration {
            identifier: Identifier { value: name, .. },
            parameters,
            ret,
            generics,
            position,
        } = func;

        let function_type = ret
            .kind
            .to_llvm_type(self.context, self.symbol_table.clone())
            .fn_type(
                parameters
                    .iter()
                    .map(|param| {
                        param
                            .kind
                            .to_llvm_type(self.context, self.symbol_table.clone())
                            .into()
                    })
                    .collect::<Vec<_>>()
                    .as_slice(),
                false,
            );

        self.module.add_function(name.as_str(), function_type, None);

        self.symbol_table.functions.insert(
            name,
            (
                function_type,
                FunctionType {
                    generics,
                    parameters,
                    ret: Box::new(ret),
                    position,
                },
            ),
        );
    }

    fn compile_literal_expression(&mut self, lit: Literal) -> (BasicValueEnum<'ctx>, TyKind) {
        match lit {
            Literal::Identifier(ident) => self.compile_identifier_expression(ident),
            Literal::Int(i) => (
                self.context
                    .i64_type()
                    .const_int(i.value as u64, false)
                    .as_basic_value_enum(),
                TyKind::Int,
            ),
            Literal::Float(f) => (
                self.context
                    .f64_type()
                    .const_float(f.value)
                    .as_basic_value_enum(),
                TyKind::Float,
            ),
            Literal::String(s) => (
                self.builder
                    .build_global_string_ptr(s.value.as_str(), ".str")
                    .as_basic_value_enum(),
                TyKind::String,
            ),
            Literal::Boolean(b) => (
                self.context
                    .bool_type()
                    .const_int(b.value as u64, false)
                    .as_basic_value_enum(),
                TyKind::Boolean,
            ),
            Literal::Array(arr) => {
                let mut values: Vec<BasicValueEnum> = Vec::new();

                for val in arr.elements {
                    values.push(self.compile_expression(val).0);
                }

                let ty = TyKind::Array(Box::new(arr.ty.clone()));
                let array_ty = ty
                    .to_llvm_type(self.context, self.symbol_table.clone())
                    .array_type(values.len() as u32);
                let ptr = self.builder.build_alloca(array_ty, "array");

                for (i, val) in values.iter().enumerate() {
                    let index = self.context.i64_type().const_int(i as u64, false);

                    let ptr = unsafe {
                        self.builder.build_in_bounds_gep(
                            self.context.i64_type(),
                            ptr,
                            &[index],
                            format!("ptr.{}", i).as_str(),
                        )
                    };

                    self.builder.build_store(ptr, *val);
                }

                (ptr.as_basic_value_enum(), ty)
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
                    values.push(self.compile_expression(val.1).0);
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

                (ptr.as_basic_value_enum(), TyKind::Custom(name))
            }
            _ => unimplemented!(),
        }
    }

    fn compile_identifier_expression(
        &mut self,
        ident: Identifier,
    ) -> (BasicValueEnum<'ctx>, TyKind) {
        let name = ident.value;

        match self.symbol_table.variables.get(&name) {
            Some((ptr, ty)) => (
                self.builder
                    .build_load(
                        ty.to_llvm_type(self.context, self.symbol_table.clone()),
                        *ptr,
                        name.as_str(),
                    )
                    .as_basic_value_enum(),
                ty.clone(),
            ),
            None => panic!("Unknown variable: {}", name),
        }
    }

    fn compile_call_expression(&mut self, call: CallExpression) -> (BasicValueEnum<'ctx>, TyKind) {
        let CallExpression {
            function: callee,
            arguments,
            ..
        } = call;

        let (function, (_, function_ty), name) = match *callee {
            Expression::Literal(Literal::Identifier(ident)) => (
                self.module.get_function(ident.value.as_str()).unwrap(),
                self.symbol_table
                    .functions
                    .get(&ident.value)
                    .unwrap()
                    .clone(),
                ident.value,
            ),
            _ => panic!("Expected identifier expression"),
        };

        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();

        for arg in arguments {
            args.push(self.compile_expression(arg).0.into());
        }

        (
            self.builder
                .build_call(function, args.as_slice(), format!("{name}.call").as_str())
                .try_as_basic_value()
                .left()
                .unwrap(),
            function_ty.ret.kind.clone(),
        )
    }

    fn compile_infix_expression(
        &mut self,
        infix: InfixExpression,
    ) -> (BasicValueEnum<'ctx>, TyKind) {
        let operator = infix.operator;
        let left = infix.left;
        let right = infix.right;

        let left = self.compile_expression(*left);
        let right = self.compile_expression(*right);

        match (left.clone().1, right.1) {
            (TyKind::Int, _) => self.compile_int_infix_expression(operator, left.0, right.0),
            (TyKind::Float, _) => self.compile_float_infix_expression(operator, left.0, right.0),
            _ => panic!("Unknown type: {:?}", left.1),
        }
    }

    fn compile_int_infix_expression(
        &mut self,
        operator: InfixOperator,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> (BasicValueEnum<'ctx>, TyKind) {
        let left = left.into_int_value();
        let right = right.into_int_value();

        (
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
            },
            TyKind::Int,
        )
    }

    fn compile_float_infix_expression(
        &mut self,
        operator: InfixOperator,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> (BasicValueEnum<'ctx>, TyKind) {
        let left = left.into_float_value();
        let right = right.into_float_value();

        (
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
            },
            TyKind::Float,
        )
    }

    fn compile_index_expression(
        &mut self,
        index: IndexExpression,
    ) -> (BasicValueEnum<'ctx>, TyKind) {
        let left = self.compile_expression(*index.clone().left);

        match left.1 {
            TyKind::Array(_) => self.compile_array_index_expression(index, left),
            TyKind::Custom(_) => self.compile_struct_index_expression(index, left),
            _ => panic!("Unknown type: {:?}", left.1),
        }
    }

    fn compile_array_index_expression(
        &mut self,
        index: IndexExpression,
        left: (BasicValueEnum<'ctx>, TyKind),
    ) -> (BasicValueEnum<'ctx>, TyKind) {
        let index = self.compile_expression(*index.index);

        match index.1 {
            TyKind::Int => {
                let element_ty = match left.1 {
                    TyKind::Array(ty) => ty.kind,
                    _ => unreachable!(),
                };
                let element_ll_ty =
                    element_ty.to_llvm_type(self.context, self.symbol_table.clone());

                let ptr = unsafe {
                    self.builder.build_gep(
                        element_ll_ty,
                        left.0.into_pointer_value(),
                        &[index.0.into_int_value()],
                        "ptr",
                    )
                };

                (
                    self.builder.build_load(element_ll_ty, ptr, "load"),
                    element_ty,
                )
            }
            _ => panic!("Unknown type: {:?}", index.1),
        }
    }

    fn compile_struct_index_expression(
        &mut self,
        index: IndexExpression,
        left: (BasicValueEnum<'ctx>, TyKind),
    ) -> (BasicValueEnum<'ctx>, TyKind) {
        let index = self.compile_expression(*index.index);

        match index.1 {
            TyKind::Int => {
                let ptr = unsafe {
                    self.builder.build_gep(
                        self.context.i64_type(),
                        left.0.into_pointer_value(),
                        &[index.0.into_int_value()],
                        "ptr",
                    )
                };

                let element_ty = match left.0 {
                    BasicValueEnum::PointerValue(ptr) => {
                        self.symbol_table.struct_fields.get(&ptr).unwrap().clone()
                    }
                    _ => unreachable!(),
                };
                (
                    self.builder
                        .build_load(self.context.i64_type(), ptr, "load"),
                    element_ty,
                )
            }
            _ => panic!("Unknown type: {:?}", index.1),
        }
    }

    fn compile_struct_statement(&mut self, stmt: StructStatement) {
        let StructStatement {
            identifier: Identifier { value: name, .. },
            generics,
            fields,
            position,
        } = stmt;

        let mut fields_ty = Vec::new();

        for (index, field) in fields.iter().enumerate() {
            fields_ty.push(
                field
                    .ty
                    .kind
                    .to_llvm_type(self.context, self.symbol_table.clone()),
            );

            // global struct.field = index
            let global = self.module.add_global(
                self.context.i64_type(),
                None,
                format!("{}.{}", name, field.identifier.value).as_str(),
            );

            global.set_initializer(&self.context.i64_type().const_int(index as u64, false));

            self.symbol_table.variables.insert(
                format!("{}.{}", name, field.identifier.value),
                (global.as_pointer_value(), field.ty.kind.clone()),
            );

            self.symbol_table
                .struct_fields
                .insert(global.as_pointer_value(), field.ty.kind.clone());
        }

        let struct_ty = self.context.opaque_struct_type(name.as_str());

        struct_ty.set_body(fields_ty.as_slice(), false);

        self.module.add_global(struct_ty, None, name.as_str());
        self.symbol_table.structs.insert(
            name,
            (
                struct_ty,
                StructType {
                    generics,
                    fields,
                    position,
                },
            ),
        );
    }
}
