use crate::{
    ast::Position,
    parser::error::{ParsingError, ParsingErrorKind},
};

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

    pub fn unexpected<T>(token: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(CompileErrorKind::Unexpected(token.to_string()), position)
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

    pub fn struct_not_found<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::StructNotFound(identifier.to_string()),
            position,
        )
    }

    pub fn function_not_found<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::FunctionNotFound(identifier.to_string()),
            position,
        )
    }

    pub fn unknown_type<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::UnknownType(identifier.to_string()),
            position,
        )
    }

    pub fn unknown_operator<T>(identifier: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            CompileErrorKind::UnknownOperator(identifier.to_string()),
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
    IndexingNonArrayType,
    ArrayElementsMustBeOfTheSameType,
    ArrayMustHaveAtLeastOneElement,
    IdentifierNotFound(String),
    StructNotFound(String),
    FunctionNotFound(String),
    UnknownType(String),
    UnknownOperator(String),
}

pub type CompileResult<T> = Result<T, CompileError>;
