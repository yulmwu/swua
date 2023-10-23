use crate::{
    ast::Position,
    parser::error::{ParsingError, ParsingErrorKind},
};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub position: Position,
}

impl CompileError {
    pub fn new(kind: CompileErrorKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn expected<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(CompileErrorKind::Expected(expected.to_string()), position)
    }

    pub fn unexpected<T>(unexpected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::Unexpected(unexpected.to_string()),
            position,
        )
    }

    pub fn type_mismatch<T>(expected: T, found: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::TypeMismatch(expected.to_string(), found.to_string()),
            position,
        )
    }

    pub fn indexing_non_array_type(position: Position) -> Self {
        Self::new(CompileErrorKind::IndexingNonArrayType, position)
    }

    pub fn array_elements_must_be_of_the_same_type(position: Position) -> Self {
        Self::new(CompileErrorKind::ArrayElementsMustBeOfTheSameType, position)
    }

    pub fn array_must_have_at_least_one_element(position: Position) -> Self {
        Self::new(CompileErrorKind::ArrayMustHaveAtLeastOneElement, position)
    }

    pub fn identifier_not_found<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::IdentifierNotFound(identifier.to_string()),
            position,
        )
    }

    pub fn variable_already_declared<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::VariableAlreadyDeclared(identifier.to_string()),
            position,
        )
    }

    pub fn struct_not_found<T>(name: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(CompileErrorKind::StructNotFound(name.to_string()), position)
    }

    pub fn function_not_found<T>(name: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::FunctionNotFound(name.to_string()),
            position,
        )
    }

    pub fn unknown_type<T>(ty: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(CompileErrorKind::UnknownType(ty.to_string()), position)
    }

    pub fn unknown_operator<T>(operator: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::UnknownOperator(operator.to_string()),
            position,
        )
    }

    pub fn if_else_must_have_the_same_type(position: Position) -> Self {
        Self::new(CompileErrorKind::IfElseMustHaveTheSameType, position)
    }
}

impl From<ParsingError> for CompileError {
    fn from(error: ParsingError) -> Self {
        Self::new(CompileErrorKind::ParsingError(error.kind), error.position)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum CompileErrorKind {
    ParsingError(ParsingErrorKind),
    Expected(String),
    Unexpected(String),
    TypeMismatch(String, String),
    IndexingNonArrayType,
    ArrayElementsMustBeOfTheSameType,
    ArrayMustHaveAtLeastOneElement,
    IdentifierNotFound(String),
    VariableAlreadyDeclared(String),
    StructNotFound(String),
    FunctionNotFound(String),
    UnknownType(String),
    UnknownOperator(String),
    IfElseMustHaveTheSameType,
}

impl fmt::Display for CompileErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParsingError(kind) => write!(f, "{kind}"),
            Self::Expected(expected) => write!(f, "Expected {expected}"),
            Self::Unexpected(unexpected) => write!(f, "Unexpected {unexpected}"),
            Self::TypeMismatch(expected, found) => {
                write!(f, "Expected {expected}, but found {found}")
            }
            Self::IndexingNonArrayType => write!(f, "Indexing non-array type"),
            Self::ArrayElementsMustBeOfTheSameType => {
                write!(f, "Array elements must be of the same type")
            }
            Self::ArrayMustHaveAtLeastOneElement => {
                write!(f, "Array must have at least one element")
            }
            Self::IdentifierNotFound(identifier) => write!(f, "Identifier {identifier} not found"),
            Self::VariableAlreadyDeclared(identifier) => {
                write!(f, "Variable {identifier} already declared")
            }
            Self::StructNotFound(name) => write!(f, "Struct {name} not found"),
            Self::FunctionNotFound(name) => write!(f, "Function {name} not found"),
            Self::UnknownType(ty) => write!(f, "Unknown type {ty}"),
            Self::UnknownOperator(operator) => write!(f, "Unknown operator {operator}"),
            Self::IfElseMustHaveTheSameType => write!(f, "If else must have the same type"),
        }
    }
}

pub type CompileResult<T> = Result<T, CompileError>;
