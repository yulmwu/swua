use crate::ast::Position;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct ParsingError {
    pub kind: ParsingErrorKind,
    pub position: Position,
}

impl ParsingError {
    pub fn new(kind: ParsingErrorKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn expected_next_token<T>(expected: T, got: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedNextToken(expected.to_string(), got.to_string()),
            position,
        )
    }

    pub fn expected_ty<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(ParsingErrorKind::ExpectedTy(expected.to_string()), position)
    }

    pub fn expected_expression<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedExpression(expected.to_string()),
            position,
        )
    }

    pub fn unexpected_token<T>(token: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::UnexpectedToken(token.to_string()),
            position,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum ParsingErrorKind {
    ExpectedNextToken(String, String),
    ExpectedTy(String),
    ExpectedExpression(String),
    UnexpectedToken(String),
}

impl fmt::Display for ParsingErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsingErrorKind::ExpectedNextToken(expected, got) => {
                write!(f, "expected `{expected}` but got `{got}`")
            }
            ParsingErrorKind::ExpectedTy(expected) => write!(f, "expected type `{expected}`"),
            ParsingErrorKind::ExpectedExpression(expected) => {
                write!(f, "expected expression `{expected}`")
            }
            ParsingErrorKind::UnexpectedToken(token) => write!(f, "unexpected token `{token}`"),
        }
    }
}

pub type ParseResult<T> = Result<T, ParsingError>;
