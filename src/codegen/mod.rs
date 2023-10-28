pub mod error;
pub mod infer;
pub mod symbol_table;
pub mod types;

use self::{
    error::{CompileError, CompileResult},
    infer::{infer_expression, infer_literal},
    symbol_table::{FunctionEntry, SymbolTable, VariableEntry},
    types::Value,
};
use crate::{ast::*, parser::Parser, tokenizer::Lexer};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicType,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
    AddressSpace, IntPredicate,
};

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

    pub fn new_with_symbol_table(
        context: &'ctx Context,
        name: &str,
        symbol_table: SymbolTable<'ctx>,
    ) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(name);

        Compiler {
            context,
            builder,
            module,
            symbol_table,
        }
    }

    pub fn compile(&mut self, source: &str) -> CompileResult<Module<'ctx>> {
        let lexer = Lexer::new(source);
        let program = match Parser::new(lexer).parse_program() {
            Ok(program) => program,
            Err(err) => return Err(CompileError::from(err[0].clone())),
        };

        let module = self.compile_module("main".to_string(), program)?;

        Ok(module)
    }

    pub fn compile_module(
        &mut self,
        name: String,
        program: Program,
    ) -> CompileResult<Module<'ctx>> {
        self.module = self.context.create_module(name.as_str());

        for stmt in program {
            self.compile_statement(stmt)?;
        }

        Ok(self.module.clone())
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

    pub fn compile_expression(&mut self, expr: Expression) -> CompileResult<Value<'ctx>> {
        let Value { value, ty } = match expr {
            Expression::AssignmentExpression(expr) => self.compile_assignment_expression(expr),
            Expression::BlockExpression(expr) => self.compile_block_expression(expr),
            Expression::PrefixExpression(expr) => self.compile_prefix_expression(expr),
            Expression::InfixExpression(expr) => self.compile_infix_expression(expr),
            Expression::IfExpression(expr) => self.compile_if_expression(expr),
            Expression::CallExpression(expr) => self.compile_call_expression(expr),
            Expression::TypeofExpression(expr) => self.compile_typeof_expression(expr),
            Expression::SizeofExpression(expr) => self.compile_sizeof_expression(expr),
            Expression::SizeofTypeExpression(expr) => self.compile_sizeof_type_expression(expr),
            Expression::IndexExpression(expr) => self.compile_index_expression(expr),
            Expression::Literal(literal) => self.compile_literal_expression(literal),
            Expression::Debug(_, _) => todo!(),
        }?;
        Ok(Value::new(value, ty.analyzed(self.context)))
    }

    fn compile_let_statement(&mut self, stmt: LetStatement) -> CompileResult<()> {
        let LetStatement {
            identifier:
                Identifier {
                    value: name,
                    position: i_position,
                },
            value: initializer,
            ty,
            position,
            ..
        } = stmt;

        if self.symbol_table.get_variable(&name).is_some() {
            return Err(CompileError::variable_already_declared(name, i_position));
        }

        let (ty, ty_pos) = match ty {
            Some(ty) => (ty.kind, Some(ty.position)),
            None if initializer.is_some() => {
                let initializer = match initializer.clone() {
                    Some(expr) => expr,
                    None => return Err(CompileError::expected("Initializer", position)),
                };
                (infer_expression(initializer, &mut self.symbol_table)?, None)
            }
            _ => todo!(),
        };

        let inferred_ty = match initializer.clone() {
            Some(expr) => infer_expression(expr, &mut self.symbol_table)?,
            None => ty.clone(),
        };

        if ty != inferred_ty {
            return Err(CompileError::type_mismatch(
                ty,
                inferred_ty,
                ty_pos.unwrap_or(position),
            ));
        }

        let alloca = self
            .builder
            .build_alloca(self.context.i64_type(), name.as_str());

        if let Some(expr) = initializer {
            let value = self.compile_expression(expr)?.value;
            self.builder.build_store(alloca, value);
        }

        self.symbol_table
            .variables()
            .insert(name, VariableEntry::new(alloca, inferred_ty));

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

        self.symbol_table.functions().insert(
            name.clone(),
            FunctionEntry::new(
                function_type,
                FunctionType {
                    generics,
                    parameters: parameters.iter().map(|param| param.ty.clone()).collect(),
                    ret: Box::new(ret),
                    position,
                },
            ),
        );

        let original_symbol_table = self.symbol_table.clone();
        self.symbol_table = SymbolTable::new_with_parent(self.symbol_table.clone());

        for (i, param) in function.get_param_iter().enumerate() {
            let alloca = self
                .builder
                .build_alloca(self.context.i64_type(), &parameters[i].identifier.value);

            self.builder.build_store(alloca, param);

            self.symbol_table.variables().insert(
                parameters[i].identifier.value.clone(),
                VariableEntry::new(alloca, parameters[i].ty.kind.clone()),
            );
        }

        for stmt in body.statements {
            match stmt {
                Statement::LetStatement(stmt) => self.compile_let_statement(stmt)?,
                Statement::FunctionDeclaration(func) => self.compile_function_declaration(func)?,
                Statement::ReturnStatement(expr) => {
                    let value = self.compile_expression(expr.value)?.value;
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

        self.symbol_table = original_symbol_table;

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

        self.symbol_table.functions().insert(
            name,
            FunctionEntry::new(
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

    fn compile_literal_expression(&mut self, lit: Literal) -> CompileResult<Value<'ctx>> {
        Ok(match lit {
            Literal::Identifier(ident) => self.compile_identifier_expression(ident)?,
            Literal::Int(i) => Value::new(
                self.context
                    .i64_type()
                    .const_int(i.value as u64, false)
                    .as_basic_value_enum(),
                TyKind::Int,
            ),
            Literal::Float(f) => Value::new(
                self.context
                    .f64_type()
                    .const_float(f.value)
                    .as_basic_value_enum(),
                TyKind::Float,
            ),
            Literal::String(s) => Value::new(
                self.builder
                    .build_global_string_ptr(s.value.as_str(), ".str")
                    .as_basic_value_enum(),
                TyKind::String,
            ),
            Literal::Boolean(b) => Value::new(
                self.context
                    .bool_type()
                    .const_int(b.value as u64, false)
                    .as_basic_value_enum(),
                TyKind::Boolean,
            ),
            Literal::Array(arr) => {
                let mut values: Vec<BasicValueEnum> = Vec::new();

                for val in arr.clone().elements {
                    values.push(self.compile_expression(val)?.value);
                }

                let ty = infer_literal(Literal::Array(arr), &mut self.symbol_table)?;
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

                Value::new(ptr.as_basic_value_enum(), ty)
            }
            Literal::Struct(struct_lit) => {
                let name = struct_lit.identifier.value;
                let struct_ty = match self.module.get_struct_type(name.as_str()) {
                    Some(ty) => ty,
                    None => return Err(CompileError::struct_not_found(name, struct_lit.position)),
                };

                let mut values: Vec<BasicValueEnum> = Vec::new();

                for val in struct_lit.fields {
                    values.push(self.compile_expression(val.1)?.value);
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

                Value::new(ptr.as_basic_value_enum(), TyKind::Custom(name))
            }
            _ => unimplemented!(),
        })
    }

    fn compile_identifier_expression(&mut self, ident: Identifier) -> CompileResult<Value<'ctx>> {
        let name = ident.value;

        Ok(match self.symbol_table.get_variable(&name) {
            Some(VariableEntry { pointer, ty }) => Value::new(
                self.builder
                    .build_load(ty.to_llvm_type(self.context), *pointer, name.as_str())
                    .as_basic_value_enum(),
                ty.clone(),
            ),
            None => return Err(CompileError::identifier_not_found(name, ident.position)),
        })
    }

    fn compile_if_expression(&mut self, expr: IfExpression) -> CompileResult<Value<'ctx>> {
        let IfExpression {
            condition,
            consequence,
            alternative,
            position,
        } = expr;

        let condition = self.compile_expression(*condition)?;
        if condition.ty != TyKind::Boolean {
            return Err(CompileError::type_mismatch(
                TyKind::Boolean,
                condition.ty,
                position,
            ));
        }

        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let then_block = self.context.append_basic_block(function, "then");
        let else_block = self.context.append_basic_block(function, "else");
        let merge_block = self.context.append_basic_block(function, "merge");

        self.builder.build_conditional_branch(
            condition.value.into_int_value(),
            then_block,
            else_block,
        );

        self.builder.position_at_end(then_block);

        let then = self.compile_expression(Expression::BlockExpression(*consequence))?;
        self.builder.build_unconditional_branch(merge_block);

        let then_block = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(else_block);

        let else_ = match alternative {
            Some(expr) => self.compile_expression(Expression::BlockExpression(*expr))?,
            None => Value::new(
                self.context
                    .i64_type()
                    .const_int(0, false)
                    .as_basic_value_enum(),
                TyKind::Void,
            ),
        };
        self.builder.build_unconditional_branch(merge_block);

        let else_block = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);

        if then.ty != else_.ty {
            return Err(CompileError::if_else_must_have_the_same_type(position));
        }

        let phi = self.builder.build_phi(then.value.get_type(), "iftmp");

        phi.add_incoming(&[(&then.value, then_block), (&else_.value, else_block)]);

        Ok(Value::new(phi.as_basic_value(), then.ty))
    }

    fn compile_call_expression(&mut self, call: CallExpression) -> CompileResult<Value<'ctx>> {
        let CallExpression {
            function: callee,
            arguments,
            position,
        } = call;

        let (function, FunctionEntry { ty, .. }, name) = {
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

                    let ty = match self.symbol_table.get_function(&ident.value) {
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
            let expr = self.compile_expression(arg.clone())?;
            args.push(expr.value.into());

            let arg_ty = infer_expression(arg, &mut self.symbol_table)?;
            if arg_ty != ty.parameters[args.len() - 1].kind {
                return Err(CompileError::type_mismatch(
                    ty.parameters[args.len() - 1].to_string(),
                    arg_ty.to_string(),
                    position,
                ));
            }
        }

        let result = match self
            .builder
            .build_call(function, args.as_slice(), format!("{name}.call").as_str())
            .try_as_basic_value()
            .left()
        {
            Some(value) => Value::new(value, ty.ret.kind),
            None => Value::new(
                self.context
                    .i64_type()
                    .const_int(0, false)
                    .as_basic_value_enum(),
                TyKind::Void,
            ),
        };

        Ok(result)
    }

    fn compile_assignment_expression(
        &mut self,
        assignment: AssignmentExpression,
    ) -> CompileResult<Value<'ctx>> {
        let AssignmentExpression {
            identifier: Identifier { value: name, .. },
            value,
            position,
        } = assignment;

        let llvm_value = self.compile_expression(*value.clone())?.value;

        Ok(match self.symbol_table.clone().get_variable(&name) {
            Some(VariableEntry { pointer, ty }) => {
                self.builder.build_store(*pointer, llvm_value);

                let inferred_ty = infer_expression(*value, &mut self.symbol_table)?;
                if inferred_ty != *ty {
                    return Err(CompileError::type_mismatch(ty, &inferred_ty, position));
                }

                Value::new(llvm_value, ty.clone())
            }
            None => return Err(CompileError::identifier_not_found(name, position)),
        })
    }

    fn compile_block_expression(&mut self, block: BlockExpression) -> CompileResult<Value<'ctx>> {
        let original_symbol_table = self.symbol_table.clone();
        self.symbol_table = SymbolTable::new_with_parent(self.symbol_table.clone());

        for stmt in block.statements {
            match stmt {
                Statement::LetStatement(stmt) => self.compile_let_statement(stmt)?,
                Statement::FunctionDeclaration(func) => self.compile_function_declaration(func)?,
                Statement::ReturnStatement(expr) => {
                    let value = self.compile_expression(expr.value.clone())?.value;

                    let result =
                        Value::new(value, infer_expression(expr.value, &mut self.symbol_table)?);
                    self.symbol_table = original_symbol_table;
                    return Ok(result);
                }
                Statement::StructStatement(stmt) => self.compile_struct_statement(stmt)?,
                Statement::ExpressionStatement(expr) => {
                    self.compile_expression(expr.expression)?;
                }
                _ => unimplemented!(),
            }
        }

        self.symbol_table = original_symbol_table;

        Ok(Value::new(
            self.context
                .i64_type()
                .const_int(0, false)
                .as_basic_value_enum(),
            TyKind::Void,
        ))
    }

    fn compile_prefix_expression(
        &mut self,
        prefix: PrefixExpression,
    ) -> CompileResult<Value<'ctx>> {
        let _prefix = prefix.clone();
        let right = _prefix.right;

        let right = self.compile_expression(*right)?;
        let operator = _prefix.operator;

        use PrefixOperator::*;

        match operator {
            Minus => match right.ty {
                TyKind::Int => Ok(Value::new(
                    self.builder
                        .build_int_neg(right.value.into_int_value(), "neg")
                        .as_basic_value_enum(),
                    TyKind::Int,
                )),
                TyKind::Float => Ok(Value::new(
                    self.builder
                        .build_float_neg(right.value.into_float_value(), "neg")
                        .as_basic_value_enum(),
                    TyKind::Float,
                )),
                _ => Err(CompileError::unknown_type(right.ty, prefix.position)),
            },
            Not => match right.ty {
                TyKind::Boolean => Ok(Value::new(
                    self.builder
                        .build_not(right.value.into_int_value(), "not")
                        .as_basic_value_enum(),
                    TyKind::Boolean,
                )),
                _ => Err(CompileError::unknown_type(right.ty, prefix.position)),
            },
        }
    }

    fn compile_infix_expression(&mut self, infix: InfixExpression) -> CompileResult<Value<'ctx>> {
        let _infix = infix.clone();
        let left = _infix.left;
        let right = _infix.right;

        let left = self.compile_expression(*left)?;
        let right = self.compile_expression(*right)?;

        use InfixOperator::*;

        match infix.operator {
            EQ | NEQ => self.compile_equality_infix_expression(infix, left, right),
            LT | GT | LTE | GTE => self.compile_comparison_infix_expression(infix, left, right),
            Plus | Minus | Asterisk | Slash | Percent => match (left.clone().ty, right.ty) {
                (TyKind::Int, _) => {
                    self.compile_int_infix_expression(infix, left.value, right.value)
                }
                (TyKind::Float, _) => {
                    self.compile_float_infix_expression(infix, left.value, right.value)
                }
                (TyKind::String, _) => todo!(),
                _ => Err(CompileError::unknown_type(left.ty, infix.position)),
            },
            _ => todo!(),
        }
    }

    fn compile_equality_infix_expression(
        &mut self,
        infix: InfixExpression,
        left: Value<'ctx>,
        right: Value<'ctx>,
    ) -> CompileResult<Value<'ctx>> {
        if left.ty != right.ty {
            return Err(CompileError::type_mismatch(
                left.ty,
                right.ty,
                infix.position,
            ));
        }

        let operator = infix.operator;

        use InfixOperator::*;

        match left.ty {
            TyKind::Int | TyKind::Boolean => {
                let left = left.value.into_int_value();
                let right = right.value.into_int_value();

                Ok(Value::new(
                    match operator {
                        EQ => self
                            .builder
                            .build_int_compare(IntPredicate::EQ, left, right, "eq")
                            .as_basic_value_enum(),
                        NEQ => self
                            .builder
                            .build_int_compare(IntPredicate::NE, left, right, "neq")
                            .as_basic_value_enum(),
                        _ => return Err(CompileError::unknown_operator(operator, infix.position)),
                    },
                    TyKind::Boolean,
                ))
            }
            TyKind::Float => {
                let left = left.value.into_float_value();
                let right = right.value.into_float_value();

                Ok(Value::new(
                    match operator {
                        EQ => self
                            .builder
                            .build_float_compare(inkwell::FloatPredicate::OEQ, left, right, "eq")
                            .as_basic_value_enum(),
                        NEQ => self
                            .builder
                            .build_float_compare(inkwell::FloatPredicate::ONE, left, right, "neq")
                            .as_basic_value_enum(),
                        _ => return Err(CompileError::unknown_operator(operator, infix.position)),
                    },
                    TyKind::Boolean,
                ))
            }
            TyKind::Array(_) | TyKind::String | TyKind::Struct(_) | TyKind::Fn(_) => todo!(),
            _ => Err(CompileError::unknown_type(left.ty, infix.position)),
        }
    }

    fn compile_comparison_infix_expression(
        &mut self,
        infix: InfixExpression,
        left: Value<'ctx>,
        right: Value<'ctx>,
    ) -> CompileResult<Value<'ctx>> {
        let left = left.value.into_int_value();
        let right = right.value.into_int_value();

        let operator = infix.operator;

        use InfixOperator::*;

        Ok(Value::new(
            match operator {
                LT => self
                    .builder
                    .build_int_compare(IntPredicate::SLT, left, right, "lt")
                    .as_basic_value_enum(),
                GT => self
                    .builder
                    .build_int_compare(IntPredicate::SGT, left, right, "gt")
                    .as_basic_value_enum(),
                LTE => self
                    .builder
                    .build_int_compare(IntPredicate::SLE, left, right, "lte")
                    .as_basic_value_enum(),
                GTE => self
                    .builder
                    .build_int_compare(IntPredicate::SGE, left, right, "gte")
                    .as_basic_value_enum(),
                _ => return Err(CompileError::unknown_operator(operator, infix.position)),
            },
            TyKind::Boolean,
        ))
    }

    fn compile_int_infix_expression(
        &mut self,
        infix: InfixExpression,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CompileResult<Value<'ctx>> {
        let left = left.into_int_value();
        let right = right.into_int_value();

        let operator = infix.operator;

        use InfixOperator::*;

        Ok(Value::new(
            match operator {
                Plus => self
                    .builder
                    .build_int_add(left, right, "add")
                    .as_basic_value_enum(),
                Minus => self
                    .builder
                    .build_int_sub(left, right, "sub")
                    .as_basic_value_enum(),
                Asterisk => self
                    .builder
                    .build_int_mul(left, right, "mul")
                    .as_basic_value_enum(),
                Slash => self
                    .builder
                    .build_int_signed_div(left, right, "div")
                    .as_basic_value_enum(),
                Percent => self
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
    ) -> CompileResult<Value<'ctx>> {
        let left = left.into_float_value();
        let right = right.into_float_value();

        let operator = infix.operator;

        use InfixOperator::*;

        Ok(Value::new(
            match operator {
                Plus => self
                    .builder
                    .build_float_add(left, right, "add")
                    .as_basic_value_enum(),
                Minus => self
                    .builder
                    .build_float_sub(left, right, "sub")
                    .as_basic_value_enum(),
                Asterisk => self
                    .builder
                    .build_float_mul(left, right, "mul")
                    .as_basic_value_enum(),
                Slash => self
                    .builder
                    .build_float_div(left, right, "div")
                    .as_basic_value_enum(),
                Percent => self
                    .builder
                    .build_float_rem(left, right, "rem")
                    .as_basic_value_enum(),
                _ => return Err(CompileError::unknown_operator(operator, infix.position)),
            },
            TyKind::Float,
        ))
    }

    fn compile_typeof_expression(&mut self, expr: TypeofExpression) -> CompileResult<Value<'ctx>> {
        let expr = self.compile_expression(*expr.expression)?;

        let ty_num = match expr.ty {
            TyKind::Int => 0,
            TyKind::Float => 1,
            TyKind::String => 2,
            TyKind::Boolean => 3,
            TyKind::Array(_) => 4,
            TyKind::Struct(_) => 5,
            TyKind::Fn(_) => 6,
            TyKind::Void => 7,
            _ => unreachable!(),
        };

        Ok(Value::new(
            self.context
                .i64_type()
                .const_int(ty_num as u64, false)
                .as_basic_value_enum(),
            TyKind::Int,
        ))
    }

    fn compile_sizeof_expression(&mut self, expr: SizeofExpression) -> CompileResult<Value<'ctx>> {
        let compiled_expr = self.compile_expression(*expr.expression)?;

        Ok(Value::new(
            match compiled_expr.ty {
                TyKind::Int | TyKind::Float | TyKind::Boolean => {
                    return self.compile_sizeof_type_expression(SizeofTypeExpression {
                        ty: Box::new(Ty::new(compiled_expr.ty.clone(), expr.position)),
                        position: expr.position,
                    })
                }
                TyKind::String => {
                    let strlen = self.module.get_function("strlen").unwrap();

                    let ptr = self.builder.build_alloca(
                        self.context.i64_type().ptr_type(AddressSpace::from(0)),
                        "ptr",
                    );

                    self.builder.build_store(ptr, compiled_expr.value);

                    let ptr = self.builder.build_load(
                        self.context.i64_type().ptr_type(AddressSpace::from(0)),
                        ptr,
                        "ptr",
                    );

                    let strlen = self
                        .builder
                        .build_call(strlen, &[ptr.as_basic_value_enum().into()], "strlen")
                        .try_as_basic_value()
                        .left()
                        .unwrap();

                    strlen
                }
                TyKind::Array(array_type) => self
                    .context
                    .i64_type()
                    .const_int(array_type.size.unwrap() as u64, false)
                    .as_basic_value_enum(),
                _ => todo!(),
            },
            TyKind::Int,
        ))
    }

    fn compile_sizeof_type_expression(
        &mut self,
        expr: SizeofTypeExpression,
    ) -> CompileResult<Value<'ctx>> {
        Ok(Value::new(
            match expr.ty.kind {
                TyKind::Int => self.context.i64_type().size_of().as_basic_value_enum(),
                TyKind::Float => self.context.f64_type().size_of().as_basic_value_enum(),
                TyKind::String => self.context.i64_type().size_of().as_basic_value_enum(),
                TyKind::Boolean => self.context.bool_type().size_of().as_basic_value_enum(),
                _ => todo!(),
            },
            TyKind::Int,
        ))
    }

    fn compile_index_expression(&mut self, index: IndexExpression) -> CompileResult<Value<'ctx>> {
        let left = self.compile_expression(*index.clone().left)?;

        match left.ty {
            TyKind::Array(_) => self.compile_array_index_expression(index, left),
            TyKind::Struct(_) => self.compile_struct_index_expression(index, left),
            _ => Err(CompileError::unknown_type(left.ty, index.position)),
        }
    }

    fn compile_array_index_expression(
        &mut self,
        index: IndexExpression,
        left: Value<'ctx>,
    ) -> CompileResult<Value<'ctx>> {
        let compiled_index = self.compile_expression(*index.index)?;

        Ok(match compiled_index.ty {
            TyKind::Int => {
                let element_ty = match left.ty {
                    TyKind::Array(array_type) => array_type.element_ty.kind,
                    _ => unreachable!(),
                };
                let element_ll_ty = element_ty.to_llvm_type(self.context);

                let ptr = unsafe {
                    self.builder.build_gep(
                        element_ll_ty,
                        left.value.into_pointer_value(),
                        &[compiled_index.value.into_int_value()],
                        "ptr",
                    )
                };

                Value::new(
                    self.builder.build_load(element_ll_ty, ptr, "load"),
                    element_ty,
                )
            }
            _ => {
                return Err(CompileError::unknown_type(
                    compiled_index.ty,
                    index.position,
                ))
            }
        })
    }

    fn compile_struct_index_expression(
        &mut self,
        _index: IndexExpression,
        _left: Value<'ctx>,
    ) -> CompileResult<Value<'ctx>> {
        todo!()
    }

    fn compile_struct_statement(&mut self, _stmt: StructStatement) -> CompileResult<()> {
        todo!()
    }
}
