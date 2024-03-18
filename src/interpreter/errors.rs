use crate::{
    parser::{ParsingError, ParsingErrorKind},
    Span,
};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct InterpretError {
    pub kind: InterpretErrorKind,
    pub help: Option<String>,
    pub span: Span,
}

macro_rules! impl_error_kind {
    (
        $(
            $ident:ident$(($($arg:ident: $ty:ty),*))?:
            $fn:ident$(<$gen:ident$(: $gen_1:tt$(+$gen_n:tt)*)?>)? $(($($param_ty:ty),*))? => $fmt:expr
        ),*
    ) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum InterpretErrorKind {
            ParsingError(ParsingErrorKind),
            $($ident$(($($ty),*))?,)*
        }

        impl fmt::Display for InterpretErrorKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Self::ParsingError(kind) => write!(f, "{kind} (Parsing Error)"),
                    $(Self::$ident$(($($arg),*))? => write!(f, $fmt),)*
                }
            }
        }

        impl InterpretError {
            pub fn new(kind: InterpretErrorKind, span: Span) -> Self {
                Self { kind, span, help: None }
            }

            pub fn set_help(mut self, help: String) -> Self {
                self.help = Some(help);
                self
            }

            pub fn parsing_error(kind: ParsingErrorKind, span: Span) -> Self {
                Self::new(InterpretErrorKind::ParsingError(kind), span)
            }

            $(
                pub fn $fn<$($gen$(: $gen_1$(+$gen_n)*)?)?>($($($arg: $param_ty,)*)? span: Span) -> Self
                {
                    Self::new(InterpretErrorKind::$ident$(($($arg.to_string()),*))?, span)
                }
            )*
        }

        impl From<ParsingError> for InterpretError {
            fn from(error: ParsingError) -> Self {
                Self::new(InterpretErrorKind::ParsingError(error.kind), error.span)
            }
        }
    };
}

impl_error_kind! {
    UndefinedIdentifier(name: String): undefined_identifier<T: ToString>(T) => "undefined identifier `{name}`",
    IdentifierAlreadyDeclared(name: String): identifier_already_declared<T: ToString>(T) => "identifier `{name}` already declared",
    InvalidBinaryOperation(op: String): invalid_binary_operation<T: ToString>(T) => "invalid binary operation `{op}`",
    InvalidUnaryOperation(op: String): invalid_unary_operation<T: ToString>(T) => "invalid unary operation `{op}`"
}

pub type InterpretResult<T> = Result<T, InterpretError>;
