use super::{Identifier, Literal, Position, Statement};
use crate::tokenizer::token::TokenKind;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    AssignmentExpression(AssignmentExpression),
    BlockExpression(BlockExpression),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    CallExpression(CallExpression),
    TypeofExpression(TypeofExpression),
    IndexExpression(IndexExpression),
    Literal(Literal),
    Debug(Box<Expression>, Position),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignmentExpression {
    pub identifier: Identifier,
    pub value: Box<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockExpression {
    pub statements: Vec<Statement>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: Box<BlockExpression>,
    pub alternative: Option<Box<BlockExpression>>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeofExpression {
    pub expression: Box<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub right: Box<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOperator {
    Minus,
    Not,
}

impl From<TokenKind<'_>> for PrefixOperator {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Minus => Self::Minus,
            TokenKind::Bang => Self::Not,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: InfixOperator,
    pub right: Box<Expression>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOperator {
    Dot,      // A.B
    Plus,     // A + B
    Minus,    // A - B
    Asterisk, // A * B
    Slash,    // A / B
    Percent,  // A % B
    EQ,       // A == B
    NEQ,      // A != B
    GT,       // A > B
    GTE,      // A >= B
    LT,       // A < B
    LTE,      // A <= B
}

impl From<TokenKind<'_>> for InfixOperator {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Dot => Self::Dot,
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Asterisk => Self::Asterisk,
            TokenKind::Slash => Self::Slash,
            TokenKind::Percent => Self::Percent,
            TokenKind::EQ => Self::EQ,
            TokenKind::NEQ => Self::NEQ,
            TokenKind::GT => Self::GT,
            TokenKind::GTE => Self::GTE,
            TokenKind::LT => Self::LT,
            TokenKind::LTE => Self::LTE,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dot => write!(f, "."),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::EQ => write!(f, "=="),
            Self::NEQ => write!(f, "!="),
            Self::GT => write!(f, ">"),
            Self::GTE => write!(f, ">="),
            Self::LT => write!(f, "<"),
            Self::LTE => write!(f, "<="),
        }
    }
}
