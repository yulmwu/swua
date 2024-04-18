use self::{
    builtins::{lookup_function, setup_builtins},
    environment::Environment,
    errors::{InterpretError, InterpretResult},
    value::{
        FunctionValue, FunctionValueParameter, StructType, StructTypeField, StructValue,
        StructValueField, Type, Value,
    },
};
use crate::{
    codegen::{
        AssignExpression, BinaryExpression, Block, CallExpression, Expression, FunctionDefinition,
        IfStatement, LetStatement, Literal, Statement, StructDeclaration, TypeofExpression,
        UnaryExpression,
    },
    preprocessor::Defines,
    BinaryOperator, Program, UnaryOperator,
};

pub mod builtins;
pub mod environment;
pub mod errors;
pub mod value;

#[derive(Debug, Default)]

pub struct Interpreter {
    pub environment: Environment,
    pub defines: Defines,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut environment = Environment::new();
        setup_builtins(&mut environment);

        Self {
            environment,
            ..Default::default()
        }
    }

    pub fn interpret(&mut self, program: Program) -> InterpretResult<Option<Value>> {
        let mut last = None;

        for statement in &program.statements.clone() {
            let result = self.interpret_statement(statement)?;
            last = result;

            if let Statement::Return(_) = statement {
                break;
            }
        }

        Ok(last)
    }

    pub fn interpret_statement(&mut self, statement: &Statement) -> InterpretResult<Option<Value>> {
        match statement {
            Statement::Expression(expression) => {
                return Ok(Some(self.interpret_expression(expression)?))
            }
            Statement::Let(stmt) => self.interpret_let(stmt)?,
            Statement::Function(stmt) => self.interpret_function_definition(stmt)?,
            Statement::ExternalFunction(_) => {
                todo!("External functions are not supported on interpreter")
            }
            Statement::Struct(stmt) => self.interpret_struct(stmt)?,
            Statement::Return(expr) => return Ok(Some(self.interpret_return(&expr.value)?)),
            Statement::If(stmt) => self.interpret_if(stmt)?,
            Statement::Type(_) => todo!(),
            Statement::While(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::Ellipsis => {
                // Do nothing
            }
        }

        Ok(None)
    }

    pub fn interpret_expression(&mut self, expression: &Expression) -> InterpretResult<Value> {
        match expression {
            Expression::Literal(literal) => self.interpret_literal(literal),
            Expression::Binary(expr) => self.interpret_binary(expr),
            Expression::Unary(expr) => self.interpret_unary(expr),
            Expression::Assign(expr) => self.interpret_assign(expr),
            Expression::Call(expr) => self.interpret_call(expr),
            Expression::Index(_) => todo!(),
            Expression::Typeof(expr) => self.interpret_typeof(expr),
            Expression::Sizeof(_) => todo!(),
            Expression::Cast(_) => todo!(),
            Expression::Pointer(_) => todo!(),
            Expression::Dereference(_) => todo!(),
            Expression::Ternary(_) => todo!(),
        }
    }

    pub fn interpret_literal(&mut self, literal: &Literal) -> InterpretResult<Value> {
        match literal {
            Literal::Identifier(name) => match self.environment.get(name.identifier.clone()) {
                Some(entry) => Ok(entry.value.clone()),
                None => Err(InterpretError::undefined_identifier(
                    name.identifier.clone(),
                    name.span,
                )),
            },
            Literal::Int(value) => Ok(Value::Int(value.value)),
            Literal::Float(value) => Ok(Value::Float(value.value)),
            Literal::String(value) => Ok(Value::String(value.value.clone())),
            Literal::Boolean(value) => Ok(Value::Boolean(value.value)),
            Literal::Array(value) => value
                .elements
                .iter()
                .map(|element| self.interpret_expression(element))
                .collect::<InterpretResult<Vec<Value>>>()
                .map(Value::Array),
            _ => todo!(),
        }
    }

    pub fn interpret_binary(&mut self, expression: &BinaryExpression) -> InterpretResult<Value> {
        let left = self.interpret_expression(&expression.left)?;
        let right = self.interpret_expression(&expression.right)?;

        match expression.operator {
            BinaryOperator::Dot => unimplemented!(),
            BinaryOperator::Plus => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left + right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left + right)),
                (Value::String(left), Value::String(right)) => {
                    Ok(Value::String(format!("{}{}", left, right)))
                }
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            BinaryOperator::Minus => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left - right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left - right)),
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            BinaryOperator::Asterisk => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left * right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left * right)),
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            BinaryOperator::Slash => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left / right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left / right)),
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            BinaryOperator::Percent => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left % right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left % right)),
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            BinaryOperator::EQ => Ok(Value::Boolean(left == right)),
            BinaryOperator::NEQ => Ok(Value::Boolean(left != right)),
            BinaryOperator::GT => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Boolean(left > right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Boolean(left > right)),
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            BinaryOperator::GTE => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Boolean(left >= right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Boolean(left >= right)),
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            BinaryOperator::LT => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Boolean(left < right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Boolean(left < right)),
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            BinaryOperator::LTE => match (left, right) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Boolean(left <= right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Boolean(left <= right)),
                _ => Err(InterpretError::invalid_binary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
        }
    }

    pub fn interpret_unary(&mut self, expression: &UnaryExpression) -> InterpretResult<Value> {
        let value = self.interpret_expression(&expression.expression)?;

        match expression.operator {
            UnaryOperator::Minus => match value {
                Value::Int(value) => Ok(Value::Int(-value)),
                Value::Float(value) => Ok(Value::Float(-value)),
                _ => Err(InterpretError::invalid_unary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
            UnaryOperator::Not => match value {
                Value::Boolean(value) => Ok(Value::Boolean(!value)),
                _ => Err(InterpretError::invalid_unary_operation(
                    expression.operator.clone(),
                    expression.span,
                )),
            },
        }
    }

    pub fn interpret_assign(&mut self, expression: &AssignExpression) -> InterpretResult<Value> {
        let value = self.interpret_expression(&expression.value)?;

        match *expression.expression.clone() {
            Expression::Literal(literal) => match literal {
                Literal::Identifier(name) => match self.environment.get(name.identifier.clone()) {
                    Some(entry) => {
                        self.environment.insert(
                            name.identifier.clone(),
                            value.clone(),
                            entry.ty.clone(),
                        );
                    }
                    None => {
                        return Err(InterpretError::undefined_identifier(
                            name.identifier.clone(),
                            name.span,
                        ));
                    }
                },
                _ => todo!(),
            },
            _ => todo!(),
        }

        Ok(value)
    }

    pub fn interpret_call(&mut self, expression: &CallExpression) -> InterpretResult<Value> {
        let args = expression
            .arguments
            .iter()
            .map(|arg| self.interpret_expression(arg))
            .collect::<InterpretResult<Vec<Value>>>()?;

        let function = self.interpret_expression(&expression.function)?;

        if let Some(builtin) = lookup_function(&function.to_string()) {
            let mut environment = Environment::new_with_parent(self.environment.clone());
            let result = builtin.call(args, &mut environment)?;
            self.environment = environment;

            return Ok(result);
        }

        match function {
            Value::Function(function) => {
                if function.parameters.len() != args.len() {
                    return Err(InterpretError::invalid_number_of_arguments(
                        function.parameters.len().to_string(),
                        args.len().to_string(),
                        expression.span,
                    ));
                }

                let mut environment = Environment::new_with_parent(self.environment.clone());

                for (parameter, arg) in function.parameters.iter().zip(args) {
                    environment.insert(parameter.name.clone(), arg, parameter.ty.clone());
                }

                let body = function.body.clone();

                let result = self.interpret_block(body, Some(environment))?;
                Ok(result.unwrap())
            }
            _ => Err(InterpretError::not_a_function(
                function.to_string(),
                expression.span,
            )),
        }
    }

    pub fn interpret_typeof(&mut self, expression: &TypeofExpression) -> InterpretResult<Value> {
        let value = self.interpret_expression(&expression.expression)?;
        let value_type: Type = value.clone().into();

        Ok(match value_type {
            Type::Int => Value::Int(0),
            Type::Float => Value::Int(1),
            Type::String => Value::Int(2),
            Type::Boolean => Value::Int(3),
            Type::Array(_) => Value::Int(4),
            Type::Struct(_) => Value::Int(5),
            Type::Function(_) => Value::Int(6),
            // Type::Void => Value::Int(7),
            // Type::Pointer(_) => Value::Int(8),
        })
    }

    pub fn interpret_let(&mut self, statement: &LetStatement) -> InterpretResult<()> {
        let LetStatement {
            name,
            value,
            ty,
            span,
        } = statement;

        let value = self.interpret_expression(value)?;
        let value_type: Type = value.clone().into();

        let ty = match ty {
            Some(ty) => {
                let ty = ty.clone().into();
                if ty != value_type {
                    return Err(InterpretError::type_mismatch(ty, value_type, *span));
                }

                ty
            }
            None => value_type.clone(),
        };

        if self
            .environment
            .insert(name.identifier.clone(), value, ty)
            .is_some()
        {
            return Err(InterpretError::identifier_already_declared(
                name.identifier.clone(),
                *span,
            ));
        }

        Ok(())
    }

    pub fn interpret_function_definition(
        &mut self,
        statement: &FunctionDefinition,
    ) -> InterpretResult<()> {
        let FunctionDefinition {
            name,
            parameters,
            body,
            return_type,
            span,
        } = statement;

        let function = Value::Function(FunctionValue {
            name: name.identifier.clone(),
            parameters: parameters
                .iter()
                .map(|param| FunctionValueParameter {
                    name: param.name.identifier.clone(),
                    ty: param.ty.clone().into(),
                    span: param.name.span,
                })
                .collect(),
            body: body.clone(),
            return_type: Box::new(return_type.clone().into()),
            span: *span,
        });

        if self
            .environment
            .insert(
                name.identifier.clone(),
                function,
                return_type.clone().into(),
            )
            .is_some()
        {
            return Err(InterpretError::identifier_already_declared(
                name.identifier.clone(),
                *span,
            ));
        }

        Ok(())
    }

    pub fn interpret_block(
        &mut self,
        block: Block,
        environment: Option<Environment>,
    ) -> InterpretResult<Option<Value>> {
        let original_environment = self.environment.clone();
        if let Some(environment) = environment {
            self.environment = environment;
        } else {
            self.environment = Environment::new_with_parent(self.environment.clone());
        }

        for statement in block.statements {
            self.interpret_statement(&statement)?;

            if let Statement::Return(expr) = statement {
                return self.interpret_expression(&expr.value).map(Some);
            }
        }

        self.environment = original_environment;

        Ok(None)
    }

    pub fn interpret_struct(&mut self, statement: &StructDeclaration) -> InterpretResult<()> {
        let StructDeclaration { name, fields, span } = statement;

        if self
            .environment
            .insert(
                name.identifier.clone(),
                Value::Struct(StructValue {
                    name: name.identifier.clone(),
                    fields: fields
                        .iter()
                        .map(|field| StructValueField {
                            name: field.0.clone(),
                            ty: field.1.ty.clone().into(),
                            span: field.1.span,
                        })
                        .collect(),
                    span: *span,
                }),
                Type::Struct(StructType {
                    name: name.identifier.clone(),
                    fields: fields
                        .iter()
                        .map(|field| StructTypeField {
                            name: field.0.clone(),
                            ty: field.1.ty.clone().into(),
                        })
                        .collect(),
                }),
            )
            .is_some()
        {
            return Err(InterpretError::identifier_already_declared(
                name.identifier.clone(),
                *span,
            ));
        }

        Ok(())
    }

    pub fn interpret_return(&mut self, expression: &Expression) -> InterpretResult<Value> {
        self.interpret_expression(expression) // TODO: Return from function
    }

    pub fn interpret_if(&mut self, statement: &IfStatement) -> InterpretResult<()> {
        let IfStatement {
            condition,
            consequence,
            alternative,
            span,
        } = statement;

        let condition = self.interpret_expression(condition)?;

        if let Value::Boolean(condition) = condition {
            if condition {
                self.interpret_block(consequence.clone(), None)?;
            } else if let Some(else_block) = alternative {
                self.interpret_block(else_block.clone(), None)?;
            }
        } else {
            return Err(InterpretError::invalid_condition_type(*span));
        }

        Ok(())
    }
}
