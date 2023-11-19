pub mod expression;
pub mod literal;
pub mod statement;

pub use expression::*;
pub use literal::*;
pub use statement::*;

use crate::{
    parser::{ParsingError, ParsingErrorKind},
    Position,
};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub position: Position,
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
            pub fn new(kind: CompileErrorKind, position: Position) -> Self {
                Self { kind, position }
            }

            pub fn parsing_error(kind: ParsingErrorKind, position: Position) -> Self {
                Self::new(CompileErrorKind::ParsingError(kind), position)
            }

            $(
                pub fn $fn<$($gen$(: $gen_1$(+$gen_n)*)?)?>($($($arg: $param_ty,)*)? position: Position) -> Self
                {
                    Self::new(CompileErrorKind::$ident$(($($arg.to_string()),*))?, position)
                }
            )*
        }

        impl From<ParsingError> for CompileError {
            fn from(error: ParsingError) -> Self {
                Self::new(CompileErrorKind::ParsingError(error.kind), error.position)
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
    IdentifierNotFound(identifier: String): identifier_not_found<T: ToString>(T) => "identifier `{identifier}` not found",
    StructNotFound(name: String): struct_not_found<T: ToString>(T) => "struct `{name}` not found",
    FieldNotFound(name: String): field_not_found<T: ToString>(T) => "field `{name}` not found",
    FunctionNotFound(name: String): function_not_found<T: ToString>(T) => "function `{name}` not found",
    FunctionMustReturnAValue: function_must_return_a_value => "function must return a value",
    UnknownSize: unknown_size => "unknown size",
    WrongNumberOfArguments(expected: String, found: String): wrong_number_of_arguments(usize, usize) => "wrong number of arguments: expected `{expected}`, found `{found}`",
    WrongNumberOfFields(expected: String, found: String): wrong_number_of_fields(usize, usize) => "wrong number of fields: expected `{expected}`, found `{found}`",
    CallNonFunctionType: call_non_function_type => "call non-function type",
    MemberAccessNonStructType: member_access_non_struct_type => "member access non-struct type",
    CannotBeAssigned: cannot_be_assigned => "cannot be assigned"
}

pub type CompileResult<T> = Result<T, CompileError>;
