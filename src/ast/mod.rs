pub mod expression;
pub mod literal;
pub mod statement;

pub use expression::*;
use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    AddressSpace,
};
pub use literal::*;
pub use statement::*;
use std::fmt;

pub type Program = Vec<Statement>;

#[derive(Debug, PartialEq, Clone)]
pub enum TyKind {
    Int,
    Float,
    String,
    Boolean,
    Array(Box<Ty>),
    Fn(FunctionType),
    Generic(Generic),
    Custom(String),
    Void,
}

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TyKind::Int | TyKind::Float | TyKind::String | TyKind::Boolean => {
                write!(f, "{self:?}")
            }
            TyKind::Array(ty) => write!(f, "{ty}[]"),
            TyKind::Fn(function_type) => write!(f, "{function_type}"),
            TyKind::Generic(generic) => write!(f, "{generic}"),
            TyKind::Custom(identifier) => write!(f, "{identifier}"),
            TyKind::Void => write!(f, "Void"),
        }
    }
}

impl TyKind {
    pub fn to_llvm_type_meta<'a>(&self, context: &'a Context) -> BasicMetadataTypeEnum<'a> {
        match self {
            TyKind::Int => BasicMetadataTypeEnum::IntType(context.i64_type()),
            TyKind::Float => BasicMetadataTypeEnum::FloatType(context.f64_type()),
            TyKind::String => BasicMetadataTypeEnum::PointerType(
                context.i8_type().ptr_type(AddressSpace::from(0)),
            ),
            TyKind::Boolean => BasicMetadataTypeEnum::IntType(context.bool_type()),
            TyKind::Array(ty) => BasicMetadataTypeEnum::ArrayType(
                ty.kind.to_llvm_type_meta(context).into_array_type(),
            ),
            _ => unimplemented!(),
        }
    }

    pub fn to_llvm_type<'a>(&self, context: &'a Context) -> BasicTypeEnum<'a> {
        match self {
            TyKind::Int => context.i64_type().into(),
            TyKind::Float => context.f64_type().into(),
            TyKind::String => context.i8_type().into(),
            TyKind::Boolean => context.bool_type().into(),
            TyKind::Array(ty) => ty
                .kind
                .to_llvm_type(context)
                .ptr_type(AddressSpace::from(0))
                .into(),
            _ => unimplemented!(),
        }
    }
}

impl From<BasicTypeEnum<'_>> for TyKind {
    fn from(basic_type: BasicTypeEnum) -> Self {
        match basic_type {
            BasicTypeEnum::IntType(_) => Self::Int,
            BasicTypeEnum::FloatType(_) => Self::Float,
            BasicTypeEnum::PointerType(_) => Self::String,
            BasicTypeEnum::ArrayType(array_type) => Self::Array(Box::new(Ty::new(
                TyKind::from(array_type.array_type(0).as_basic_type_enum()),
                Position(0, 0),
            ))),
            _ => unimplemented!(),
        }
    }
}

pub type IdentifierGeneric = Vec<Identifier>;

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let parameters = self
            .parameters
            .iter()
            .map(|parameter| parameter.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let generics = match &self.generics {
            Some(generics) => {
                let generics = generics
                    .iter()
                    .map(|generic| generic.value.clone())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("<{generics}>")
            }
            None => String::new(),
        };
        write!(f, "fn{generics}({parameters}) -> {}", self.return_type)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Generic(pub Box<Ty>, pub Vec<Ty>);

impl Generic {
    pub fn new(ty: Ty, generic_types: Vec<Ty>) -> Self {
        Generic(Box::new(ty), generic_types)
    }
}

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let generic_types = self
            .1
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}<{generic_types}>", self.0)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Position(pub usize, pub usize);

impl PartialEq for Position {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}:{}", self.0, self.1)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub position: Position,
}

impl Ty {
    pub fn new(ty: TyKind, position: Position) -> Self {
        Self { kind: ty, position }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub generics: Option<IdentifierGeneric>,
    pub parameters: Vec<Ty>,
    pub return_type: Box<Ty>,
    pub position: Position,
}

#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum Priority {
    Lowest,
    Dot,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}
