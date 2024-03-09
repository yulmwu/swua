use self::{
    environment::Environment,
    errors::{InterpretError, InterpretResult},
    value::{FunctionValue, FunctionValueParameter, Value},
};
use crate::codegen::{Block, Expression, FunctionDefinition, LetStatement, Literal, Statement};

pub mod environment;
pub mod errors;
pub mod value;

#[derive(Debug, Default)]

pub struct Interpreter {
    pub environment: Environment,
    pub program: Vec<Statement>,
}

impl Interpreter {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn interpret(&mut self) -> InterpretResult<()> {
        for statement in &self.program.clone() {
            self.interpret_statement(statement)?;
        }

        Ok(())
    }

    /*
    Expression(Expression),
    Let(LetStatement),
    Function(FunctionDefinition),
    ExternalFunction(ExternalFunctionDeclaration),
    Struct(StructDeclaration),
    Return(ReturnStatement),
    If(IfStatement),
    Type(TypeDeclaration),
    While(While),
    For(For),
    Ellipsis,
     */

    pub fn interpret_statement(&mut self, statement: &Statement) -> InterpretResult<()> {
        match statement {
            Statement::Expression(expression) => {
                self.interpret_expression(expression)?;
            }
            Statement::Let(stmt) => self.interpret_let(stmt)?,
            Statement::Function(stmt) => self.interpret_function_definition(stmt)?,
            Statement::ExternalFunction(_) => {
                todo!("External functions are not supported on interpreter")
            }
            Statement::Struct(_) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::If(_) => todo!(),
            Statement::Type(_) => todo!(),
            Statement::While(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::Ellipsis => {
                // Do nothing
            }
        }

        Ok(())
    }

    pub fn interpret_expression(&mut self, expression: &Expression) -> InterpretResult<Value> {
        match expression {
            Expression::Literal(literal) => self.interpret_literal(literal),
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
            .is_none()
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
            .is_none()
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
}
