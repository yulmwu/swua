use crate::Span;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
#[rustfmt::skip]
pub enum TokenKind {
    Indent, Dedent, Newline, EOF,

    Identifier(String), Int(i64), Float(f64), String(String), Boolean(bool), Comment,

    Assign, Plus, Minus, Bang, Asterisk, Slash, Percent, Arrow, DoubleArrow, Ampersand, At, Sharp, Pipe,

    Dot, Comma, Colon, DoubleColon, Semicolon,

    LParen, RParen, LBrace, RBrace, LBracket, RBracket,

    LT, GT, LTE, GTE, EQ, NEQ,

    Let, Define, If, Else, Return, Type, Declare, Struct, While, As,

    IntType, FloatType, StringType, BooleanType, VoidType,

    Extern, Typeof, Sizeof,
}

impl From<&str> for TokenKind {
    fn from(s: &str) -> Self {
        match s {
            "let" => TokenKind::Let,
            "define" => TokenKind::Define,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "type" => TokenKind::Type,
            "declare" => TokenKind::Declare,
            "struct" => TokenKind::Struct,
            "while" => TokenKind::While,
            "as" => TokenKind::As,
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            "int" => TokenKind::IntType,
            "float" => TokenKind::FloatType,
            "str" => TokenKind::StringType,
            "bool" => TokenKind::BooleanType,
            "void" => TokenKind::VoidType,
            "extern" => TokenKind::Extern,
            "typeof" => TokenKind::Typeof,
            "sizeof" => TokenKind::Sizeof,
            s => TokenKind::Identifier(s.to_string()),
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro_rules! to_s {
            ($( $x:ident )*) => {
                match &self {
                    $( TokenKind::$x(x) => x.to_string(), )*
                    _ => format!("{:?}", self)
                }
            }
        }

        write!(f, "{}", to_s! { Identifier String Int Float Boolean })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {} at {:?}", self.kind, self.span)
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::EOF,
            span: Span::default(),
        }
    }
}
