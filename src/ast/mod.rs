pub mod expression;
pub mod literal;
pub mod statement;

use crate::codegen::error::{CompileError, CompileResult};
pub use expression::*;
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    values::IntValue,
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
    Array(ArrayType),
    Fn(FunctionType),
    Struct(StructType),
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
            TyKind::Array(array_type) => write!(f, "{array_type}"),
            TyKind::Fn(function_type) => write!(f, "{function_type}"),
            TyKind::Struct(struct_type) => write!(f, "{struct_type}"),
            TyKind::Generic(generic) => write!(f, "{generic}"),
            TyKind::Custom(identifier) => write!(f, "{identifier}"),
            TyKind::Void => write!(f, "Void"),
        }
    }
}

impl TyKind {
    pub fn to_llvm_type<'a>(&self, context: &'a Context) -> BasicTypeEnum<'a> {
        match self {
            TyKind::Int => context.i64_type().into(),
            TyKind::Float => context.f64_type().into(),
            TyKind::String => context.i8_type().ptr_type(AddressSpace::from(0)).into(),
            TyKind::Boolean => context.bool_type().into(),
            TyKind::Array(array_type) => array_type
                .element_ty
                .kind
                .to_llvm_type(context)
                .ptr_type(AddressSpace::from(0))
                .into(),
            ty => unimplemented!("{ty:?}"),
        }
    }

    pub fn analyzed(&self, _: &Context) -> TyKind {
        match self {
            TyKind::Custom(_) => {
                todo!()
            }
            other => other.clone(),
        }
    }

    pub fn size_of<'a>(
        &self,
        context: &'a Context,
        position: Position,
    ) -> CompileResult<IntValue<'a>> {
        Ok(match self.to_llvm_type(context).size_of() {
            Some(size) => size,
            None => return Err(CompileError::unknown_size(position)),
        })
    }
}

pub type IdentifierGeneric = Vec<Identifier>;

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

    pub fn size_of<'a>(&self, context: &'a Context) -> CompileResult<IntValue<'a>> {
        self.kind.size_of(context, self.position)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub element_ty: Box<Ty>,
    pub size: Option<usize>,
    pub position: Position,
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}[{}]",
            self.element_ty,
            if let Some(size) = &self.size {
                size.to_string()
            } else {
                String::new()
            }
        )
    }
}

impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.element_ty == other.element_ty
            && match (&self.size, &other.size) {
                (Some(size), Some(other_size)) => size == other_size,
                _ => true,
            }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub generics: Option<IdentifierGeneric>,
    pub parameters: Vec<Ty>,
    pub ret: Box<Ty>,
    pub position: Position,
}

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
        write!(f, "fn{generics}({parameters}) -> {}", self.ret)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub identifier: Identifier,
    pub generics: Option<IdentifierGeneric>,
    pub fields: Vec<StructField>,
    pub position: Position,
}

impl fmt::Display for StructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let fields = self
            .fields
            .iter()
            .map(ToString::to_string)
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
        write!(
            f,
            "struct {}{generics} {{ {fields} }}",
            self.identifier.value
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub identifier: Identifier,
    pub ty: Ty,
    pub position: Position,
}

impl fmt::Display for StructField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}: {}", self.identifier.value, self.ty)
    }
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
