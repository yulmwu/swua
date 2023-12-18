pub mod expression;
pub mod literal;
pub mod statement;

pub use expression::*;
pub use literal::*;
pub use statement::*;

pub mod symbol_table;
pub mod types;

use crate::{
    lexer::LexingError,
    parser::{ParsingError, ParsingErrorKind},
    Span,
};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct CompileError {
    pub kind: CompileErrorKind,
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
        pub enum CompileErrorKind {
            ParsingError(ParsingErrorKind),
            $($ident$(($($ty),*))?,)*
        }

        impl fmt::Display for CompileErrorKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Self::ParsingError(kind) => write!(f, "{kind} (Parsing Error)"),
                    $(Self::$ident$(($($arg),*))? => write!(f, $fmt),)*
                }
            }
        }

        impl CompileError {
            pub fn new(kind: CompileErrorKind, span: Span) -> Self {
                Self { kind, span }
            }

            pub fn parsing_error(kind: ParsingErrorKind, span: Span) -> Self {
                Self::new(CompileErrorKind::ParsingError(kind), span)
            }

            $(
                pub fn $fn<$($gen$(: $gen_1$(+$gen_n)*)?)?>($($($arg: $param_ty,)*)? span: Span) -> Self
                {
                    Self::new(CompileErrorKind::$ident$(($($arg.to_string()),*))?, span)
                }
            )*
        }

        impl From<ParsingError> for CompileError {
            fn from(error: ParsingError) -> Self {
                Self::new(CompileErrorKind::ParsingError(error.kind), error.span)
            }
        }

        impl From<LexingError> for CompileError {
            fn from(error: LexingError) -> Self {
                ParsingError::from(error).into()
            }
        }
    };
}

impl_error_kind! {
    Expected(expected: String): expected<T: ToString>(T) => "expected `{expected}`",
    Unexpected(unexpected: String): unexpected<T: ToString>(T) => "unexpected `{unexpected}`",
    TypeMismatch(expected: String, found: String): type_mismatch<T: ToString>(T, T) => "expected `{expected}`, but found `{found}`",
    TypeThatCannotBeIndexed: type_that_cannot_be_indexed => "type that cannot be indexed",
    ArrayMustHaveAtLeastOneElement: array_must_have_at_least_one_element => "array must have at least one element",
    VariableAlreadyDeclared(identifier: String): variable_already_declared<T: ToString>(T) => "variable `{identifier}` already declared",
    FunctionAlreadyDeclared(identifier: String): function_already_declared<T: ToString>(T) => "function `{identifier}` already declared",
    StructAlreadyDeclared(identifier: String): struct_already_declared<T: ToString>(T) => "struct `{identifier}` already declared",
    TypeAlreadyDeclared(identifier: String): type_already_declared<T: ToString>(T) => "type `{identifier}` already declared",
    IdentifierNotFound(identifier: String): identifier_not_found<T: ToString>(T) => "identifier `{identifier}` not found",
    StructNotFound(name: String): struct_not_found<T: ToString>(T) => "struct `{name}` not found",
    FieldNotFound(name: String): field_not_found<T: ToString>(T) => "field `{name}` not found",
    FunctionNotFound(name: String): function_not_found<T: ToString>(T) => "function `{name}` not found",
    TypeNotFound(name: String): type_not_found<T: ToString>(T) => "type `{name}` not found",
    UnknownSize: unknown_size => "unknown size",
    WrongNumberOfArguments(expected: String, found: String): wrong_number_of_arguments(usize, usize) => "wrong number of arguments: expected `{expected}`, found `{found}`",
    WrongNumberOfFields(expected: String, found: String): wrong_number_of_fields(usize, usize) => "wrong number of fields: expected `{expected}`, found `{found}`",
    CallNonFunctionType: call_non_function_type => "call non-function type",
    MemberAccessNonStructType: member_access_non_struct_type => "member access non-struct type",
    CannotBeAssigned: cannot_be_assigned => "cannot be assigned",
    ElseClauseIsRequired: else_clause_is_required => "else clause is required"
}

pub type CompileResult<T> = Result<T, CompileError>;
