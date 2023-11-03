pub mod expression;
pub mod literal;
pub mod statement;

use crate::codegen::{
    error::{CompileError, CompileResult},
    symbol_table::SymbolTable,
};
pub use expression::*;
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    values::IntValue,
    AddressSpace,
};
pub use literal::*;
pub use statement::*;
use std::{collections::HashMap, fmt};

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
            TyKind::Custom(identifier) => write!(f, "{identifier}"),
            TyKind::Void => write!(f, "Void"),
        }
    }
}

impl TyKind {
    pub fn to_llvm_type<'a>(
        &self,
        context: &'a Context,
        symbol_table: &mut SymbolTable,
    ) -> BasicTypeEnum<'a> {
        match self.analyze(symbol_table).unwrap() {
            TyKind::Int => context.i64_type().into(),
            TyKind::Float => context.f64_type().into(),
            TyKind::String => context.i8_type().ptr_type(AddressSpace::from(0)).into(),
            TyKind::Boolean => context.bool_type().into(),
            TyKind::Array(array_type) => array_type
                .element_ty
                .kind
                .to_llvm_type(context, symbol_table)
                .ptr_type(AddressSpace::from(0))
                .into(),
            TyKind::Struct(struct_type) => context
                .struct_type(
                    &struct_type
                        .fields
                        .iter()
                        .map(|field| field.1 .1.kind.to_llvm_type(context, symbol_table))
                        .collect::<Vec<_>>(),
                    false,
                )
                .ptr_type(AddressSpace::from(0))
                .into(),
            _ => unimplemented!(),
        }
    }

    pub fn analyze(&self, symbol_table: &mut SymbolTable) -> CompileResult<TyKind> {
        Ok(match self {
            TyKind::Array(array_type) => {
                let element_ty = array_type.element_ty.analyze(symbol_table)?;
                TyKind::Array(ArrayType {
                    element_ty: Box::new(element_ty),
                    size: array_type.size,
                    position: array_type.position,
                })
            }
            TyKind::Custom(identifier) => {
                let struct_type = symbol_table.get_struct(identifier).unwrap();
                TyKind::Struct(struct_type.ty.clone())
            }
            other => other.clone(),
        })
    }

    pub fn size_of<'a>(
        &self,
        context: &'a Context,
        symbol_table: &mut SymbolTable,
        position: Position,
    ) -> CompileResult<IntValue<'a>> {
        Ok(match self.to_llvm_type(context, symbol_table).size_of() {
            Some(size) => size,
            None => return Err(CompileError::unknown_size(position)),
        })
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

    pub fn analyze(&self, symbol_table: &mut SymbolTable) -> CompileResult<Ty> {
        Ok(Self {
            kind: self.kind.analyze(symbol_table)?,
            position: self.position,
        })
    }

    pub fn size_of<'a>(
        &self,
        context: &'a Context,
        symbol_table: &mut SymbolTable,
    ) -> CompileResult<IntValue<'a>> {
        self.kind.size_of(context, symbol_table, self.position)
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

        write!(f, "fn({parameters}) -> {}", self.ret)
    }
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub identifier: String,
    pub fields: HashMap<String, (usize, Ty)>,
    pub position: Position,
}

impl fmt::Display for StructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let fields = self
            .fields
            .iter()
            .map(|(identifier, ty)| format!("{identifier}: {}", ty.1))
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "struct {} {{ {fields} }}", self.identifier)
    }
}

impl PartialEq for StructType {
    fn eq(&self, other: &Self) -> bool {
        let field_self = self
            .fields
            .iter()
            .map(|(identifier, ty)| (identifier, &ty.1))
            .collect::<HashMap<_, _>>();
        let field_other = other
            .fields
            .iter()
            .map(|(identifier, ty)| (identifier, &ty.1))
            .collect::<HashMap<_, _>>();

        self.identifier == other.identifier && field_self == field_other
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
