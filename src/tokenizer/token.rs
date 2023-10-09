use crate::ast::Position;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
#[rustfmt::skip]
pub enum TokenKind<'a> {
    ILLEGAL(char), EOF, IDENT(&'a str),

    Number(f64), String(&'a str), Boolean(bool), Comment,

    Assign, Plus, Minus, Bang, Asterisk, Slash, Percent, Arrow, DoubleArrow,

    Dot, Comma, Colon, Semicolon,

    LParen, RParen, LBrace, RBrace, LBracket, RBracket,

    LT, GT, LTE, GTE, EQ, NEQ,

    Let, Mut, If, Else, Return, Function, Type, Declare, Struct, Typeof, Spread,

    NumberType, StringType, BooleanType,

    Debug
}

impl<'a> From<&'a str> for TokenKind<'a> {
    fn from(s: &'a str) -> Self {
        match s {
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "fn" => TokenKind::Function,
            "type" => TokenKind::Type,
            "declare" => TokenKind::Declare,
            "struct" => TokenKind::Struct,
            "typeof" => TokenKind::Typeof,
            "spread" => TokenKind::Spread,
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            "number" => TokenKind::NumberType,
            "string" => TokenKind::StringType,
            "boolean" => TokenKind::BooleanType,
            "debug" => TokenKind::Debug,
            s => TokenKind::IDENT(s),
        }
    }
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro_rules! to_s {
            ($( $x:ident )*) => {
                match &self {
                    $( TokenKind::$x(x) => x.to_string(), )*
                    _ => format!("{:?}", self)
                }
            }
        }

        write!(f, "{}", to_s! { IDENT String Number Boolean })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub position: Position,
}

impl Default for Token<'_> {
    fn default() -> Self {
        Token::new(TokenKind::ILLEGAL('\0'), Position(0, 0))
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {:?} at {:?}", self.kind, self.position)
    }
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, position: Position) -> Self {
        Token { kind, position }
    }
}
