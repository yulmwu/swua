pub mod error;
pub mod infer;

use self::{
    error::{CompileError, CompileResult},
    infer::{infer_expression, infer_literal},
};
use crate::ast::*;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{self, BasicType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
};
use std::collections::HashMap;

/// (llvm value, type)
type ExpressionReturn<'ctx> = (BasicValueEnum<'ctx>, TyKind);

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    /// Variable name, (llvm pointer, type)
    pub variables: HashMap<String, (PointerValue<'a>, TyKind)>,
    /// Function name, (llvm function type, function type)
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

    pub fn compile_module(&mut self, name: String, program: Program) -> CompileResult<()> {
        self.module = self.context.create_module(name.as_str());

        for stmt in program {
            self.compile_statement(stmt)?;
        }

        Ok(())
    }

    pub fn compile_statement(&mut self, stmt: Statement) -> CompileResult<()> {
        match stmt {
            Statement::LetStatement(stmt) => self.compile_let_statement(stmt)?,
            Statement::FunctionDeclaration(func) => self.compile_function_declaration(func)?,
            Statement::ExternFunctionDeclaration(func) => {
                self.compile_external_function_declaration(func)?
            }
            Statement::StructStatement(stmt) => self.compile_struct_statement(stmt)?,
            Statement::ExpressionStatement(expr) => {
                self.compile_expression(expr.expression)?;
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    pub fn compile_expression(
        &mut self,
        expr: Expression,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let (value, ty) = match expr {
            Expression::AssignmentExpression(expr) => self.compile_assignment_expression(expr),
            Expression::BlockExpression(_) => todo!(),
            Expression::PrefixExpression(_) => todo!(),
            Expression::InfixExpression(expr) => self.compile_infix_expression(expr),
            Expression::IfExpression(_) => todo!(),
            Expression::CallExpression(expr) => self.compile_call_expression(expr),
            Expression::TypeofExpression(_) => todo!(),
            Expression::IndexExpression(expr) => self.compile_index_expression(expr),
            Expression::Literal(literal) => self.compile_literal_expression(literal),
            Expression::Debug(_, _) => todo!(),
        }?;
        Ok((value, ty.analyzed(self.context)))
    }

    fn compile_let_statement(&mut self, stmt: LetStatement) -> CompileResult<()> {
        let LetStatement {
            identifier: Identifier { value: name, .. },
            value: initializer,
            ty,
            position,
            ..
        } = stmt;

        let ty = match ty {
            Some(ty) => ty.kind,
            None if initializer.is_some() => {
                let initializer = match initializer.clone() {
                    Some(expr) => expr,
                    None => return Err(CompileError::expected("Initializer", position)),
                };
                infer_expression(initializer, &self.symbol_table)?
            }
            _ => todo!(),
        };

        let alloca = self
            .builder
            .build_alloca(self.context.i64_type(), name.as_str());

        if let Some(expr) = initializer {
            let value = self.compile_expression(expr)?.0;
            self.builder.build_store(alloca, value);
        }

        self.symbol_table.variables.insert(name, (alloca, ty));

        Ok(())
    }

    fn compile_function_declaration(&mut self, func: FunctionDeclaration) -> CompileResult<()> {
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
            .map(|param| param.ty.kind.to_llvm_type(self.context).into())
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
                Statement::LetStatement(stmt) => self.compile_let_statement(stmt)?,
                Statement::FunctionDeclaration(func) => self.compile_function_declaration(func)?,
                Statement::ReturnStatement(expr) => {
                    let value = self.compile_expression(expr.value)?.0;
                    self.builder.build_return(Some(&value));

                    break;
                }
                Statement::StructStatement(stmt) => self.compile_struct_statement(stmt)?,
                Statement::ExpressionStatement(expr) => {
                    self.compile_expression(expr.expression)?;
                }
                _ => unimplemented!(),
            }
        }

        Ok(())
    }

    fn compile_external_function_declaration(
        &mut self,
        func: ExternFunctionDeclaration,
    ) -> CompileResult<()> {
        let ExternFunctionDeclaration {
            identifier: Identifier { value: name, .. },
            parameters,
            ret,
            generics,
            position,
        } = func;

        let function_type = ret.kind.to_llvm_type(self.context).fn_type(
            parameters
                .iter()
                .map(|param| param.kind.to_llvm_type(self.context).into())
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

        Ok(())
    }

    fn compile_literal_expression(
        &mut self,
        lit: Literal,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        Ok(match lit {
            Literal::Identifier(ident) => self.compile_identifier_expression(ident)?,
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

                for val in arr.clone().elements {
                    values.push(self.compile_expression(val)?.0);
                }

                let ty = infer_literal(Literal::Array(arr), &self.symbol_table)?;
                let array_ty = ty
                    .to_llvm_type(self.context)
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
            Literal::Struct(struct_lit) => {
                let name = struct_lit.identifier.value;
                let struct_ty = match self.module.get_struct_type(name.as_str()) {
                    Some(ty) => ty,
                    None => return Err(CompileError::struct_not_found(name, struct_lit.position)),
                };

                let mut values: Vec<BasicValueEnum> = Vec::new();

                for val in struct_lit.fields {
                    values.push(self.compile_expression(val.1)?.0);
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
        })
    }

    fn compile_identifier_expression(
        &mut self,
        ident: Identifier,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let name = ident.value;

        Ok(match self.symbol_table.variables.get(&name) {
            Some((ptr, ty)) => (
                self.builder
                    .build_load(ty.to_llvm_type(self.context), *ptr, name.as_str())
                    .as_basic_value_enum(),
                ty.clone(),
            ),
            None => return Err(CompileError::identifier_not_found(name, ident.position)),
        })
    }

    fn compile_call_expression(
        &mut self,
        call: CallExpression,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let CallExpression {
            function: callee,
            arguments,
            position,
        } = call;

        let (function, (_, function_ty), name) = {
            match *callee {
                Expression::Literal(Literal::Identifier(ident)) => {
                    let function = match self.module.get_function(ident.value.as_str()) {
                        Some(function) => function,
                        None => {
                            return Err(CompileError::function_not_found(
                                ident.value,
                                ident.position,
                            ))
                        }
                    };

                    let ty = match self.symbol_table.functions.get(&ident.value) {
                        Some(ty) => ty.clone(),
                        None => {
                            return Err(CompileError::function_not_found(
                                ident.value,
                                ident.position,
                            ))
                        }
                    };
                    (function, ty, ident.value)
                }
                _ => return Err(CompileError::expected("identifier", position)),
            }
        };

        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();

        for arg in arguments {
            args.push(self.compile_expression(arg)?.0.into());
        }

        let llvm_value = match self
            .builder
            .build_call(function, args.as_slice(), format!("{name}.call").as_str())
            .try_as_basic_value()
            .left()
        {
            Some(value) => value,
            None => return Err(CompileError::expected("return value", position)),
        };

        Ok((llvm_value, function_ty.ret.kind.clone()))
    }

    fn compile_assignment_expression(
        &mut self,
        assignment: AssignmentExpression,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let AssignmentExpression {
            identifier: Identifier { value: name, .. },
            value,
            position,
        } = assignment;

        let llvm_value = self.compile_expression(*value.clone())?.0;

        Ok(match self.symbol_table.variables.get(&name) {
            Some((ptr, ty)) => {
                self.builder.build_store(*ptr, llvm_value);

                assert_eq!(ty.clone(), infer_expression(*value, &self.symbol_table)?);

                (llvm_value, ty.clone())
            }
            None => return Err(CompileError::identifier_not_found(name, position)),
        })
    }

    fn compile_infix_expression(
        &mut self,
        infix: InfixExpression,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let _infix = infix.clone();
        let left = _infix.left;
        let right = _infix.right;

        let left = self.compile_expression(*left)?;
        let right = self.compile_expression(*right)?;

        match (left.clone().1, right.1) {
            (TyKind::Int, _) => self.compile_int_infix_expression(infix, left.0, right.0),
            (TyKind::Float, _) => self.compile_float_infix_expression(infix, left.0, right.0),
            _ => Err(CompileError::unknown_type(left.1, infix.position)),
        }
    }

    fn compile_int_infix_expression(
        &mut self,
        infix: InfixExpression,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let left = left.into_int_value();
        let right = right.into_int_value();

        let operator = infix.operator;

        Ok((
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
                _ => return Err(CompileError::unknown_operator(operator, infix.position)),
            },
            TyKind::Int,
        ))
    }

    fn compile_float_infix_expression(
        &mut self,
        infix: InfixExpression,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let left = left.into_float_value();
        let right = right.into_float_value();

        let operator = infix.operator;

        Ok((
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
                _ => return Err(CompileError::unknown_operator(operator, infix.position)),
            },
            TyKind::Float,
        ))
    }

    fn compile_index_expression(
        &mut self,
        index: IndexExpression,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let left = self.compile_expression(*index.clone().left)?;

        match left.1 {
            TyKind::Array(_) => self.compile_array_index_expression(index, left),
            TyKind::Struct(_) => self.compile_struct_index_expression(index, left),
            _ => Err(CompileError::unknown_type(left.1, index.position)),
        }
    }

    fn compile_array_index_expression(
        &mut self,
        index: IndexExpression,
        left: ExpressionReturn<'ctx>,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        let compiled_index = self.compile_expression(*index.index)?;

        Ok(match compiled_index.1 {
            TyKind::Int => {
                let element_ty = match left.1 {
                    TyKind::Array(ty) => ty.kind,
                    _ => unreachable!(),
                };
                let element_ll_ty = element_ty.to_llvm_type(self.context);

                let ptr = unsafe {
                    self.builder.build_gep(
                        element_ll_ty,
                        left.0.into_pointer_value(),
                        &[compiled_index.0.into_int_value()],
                        "ptr",
                    )
                };

                (
                    self.builder.build_load(element_ll_ty, ptr, "load"),
                    element_ty,
                )
            }
            _ => return Err(CompileError::unknown_type(compiled_index.1, index.position)),
        })
    }

    fn compile_struct_index_expression(
        &mut self,
        _index: IndexExpression,
        _left: ExpressionReturn<'ctx>,
    ) -> CompileResult<ExpressionReturn<'ctx>> {
        todo!()
    }

    fn compile_struct_statement(&mut self, _stmt: StructStatement) -> CompileResult<()> {
        todo!()
    }
}
