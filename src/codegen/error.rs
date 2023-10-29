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

    pub fn unknown_size(position: Position) -> Self {
        Self::new(CompileErrorKind::UnknownSize, position)
    }

    pub fn if_else_must_have_the_same_type(position: Position) -> Self {
        Self::new(CompileErrorKind::IfElseMustHaveTheSameType, position)
    }

    pub fn wrong_number_of_arguments(expected: usize, found: usize, position: Position) -> Self {
        Self::new(
            CompileErrorKind::WrongNumberOfArguments(expected, found),
            position,
        )
    }

    pub fn call_non_function_type<T>(ty: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::CallNonFunctionType(ty.to_string()),
            position,
        )
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
    UnknownSize,
    IfElseMustHaveTheSameType,
    WrongNumberOfArguments(usize, usize),
    CallNonFunctionType(String),
}

impl fmt::Display for CompileErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParsingError(kind) => write!(f, "{kind}"),
            Self::Expected(expected) => write!(f, "expected `{expected}`"),
            Self::Unexpected(unexpected) => write!(f, "unexpected `{unexpected}`"),
            Self::TypeMismatch(expected, found) => {
                write!(f, "expected `{expected}`, but found `{found}`")
            }
            Self::IndexingNonArrayType => write!(f, "indexing non-array type"),
            Self::ArrayElementsMustBeOfTheSameType => {
                write!(f, "array elements must be of the same type")
            }
            Self::ArrayMustHaveAtLeastOneElement => {
                write!(f, "array must have at least one element")
            }
            Self::IdentifierNotFound(identifier) => {
                write!(f, "identifier `{identifier}` not found")
            }
            Self::VariableAlreadyDeclared(identifier) => {
                write!(f, "variable `{identifier}` already declared")
            }
            Self::StructNotFound(name) => write!(f, "struct `{name}` not found"),
            Self::FunctionNotFound(name) => write!(f, "function `{name}` not found"),
            Self::UnknownType(ty) => write!(f, "unknown type `{ty}`"),
            Self::UnknownOperator(operator) => write!(f, "unknown operator `{operator}`"),
            Self::UnknownSize => write!(f, "unknown size"),
            Self::IfElseMustHaveTheSameType => write!(f, "if else must have the same type"),
            Self::WrongNumberOfArguments(expected, found) => write!(
                f,
                "wrong number of arguments: expected `{expected}`, found `{found}`"
            ),
            Self::CallNonFunctionType(ty) => write!(f, "call non-function type `{ty}`"),
        }
    }
}

pub type CompileResult<T> = Result<T, CompileError>;
