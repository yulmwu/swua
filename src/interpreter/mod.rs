use self::{
    environment::Environment,
    errors::{InterpretError, InterpretResult},
    value::{FunctionValue, FunctionValueParameter, Value},
};
use crate::{
    codegen::{
        BinaryExpression, Block, Expression, FunctionDefinition, LetStatement, Literal, Statement,
    },
    BinaryOperator, Program,
};

pub mod environment;
pub mod errors;
pub mod value;

#[derive(Debug, Default)]

pub struct Interpreter {
    pub environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Default::default()
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
            Statement::Struct(_) => todo!(),
            Statement::Return(expr) => return Ok(Some(self.interpret_return(&expr.value)?)),
            Statement::If(_) => todo!(),
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
            _ => todo!(),
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

    pub fn interpret_let(&mut self, statement: &LetStatement) -> InterpretResult<()> {
        let LetStatement {
            name,
            value,
            ty,
            span,
        } = statement;

        let value = self.interpret_expression(value)?;

        let ty = match ty {
            Some(ty) => ty.clone(),
            None => unimplemented!(),
        };

        if self
            .environment
            .insert(name.identifier.clone(), value, ty.into())
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

    pub fn interpret_block(&mut self, block: Block) -> InterpretResult<Option<Value>> {
        let environment = self.environment.clone();
        self.environment = Environment::new_with_parent(self.environment.clone());

        for statement in block.statements {
            self.interpret_statement(&statement)?;

            if let Statement::Return(expr) = statement {
                return self.interpret_expression(&expr.value).map(Some);
            }
        }

        self.environment = environment;

        Ok(None)
    }

    pub fn interpret_return(&mut self, expression: &Expression) -> InterpretResult<Value> {
        self.interpret_expression(expression)
    }
}
