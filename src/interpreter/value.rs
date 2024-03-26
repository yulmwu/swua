use std::fmt;

use crate::{
    codegen::{
        types::{AstType, AstTypeKind},
        Block,
    },
    Span,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
    Function(FunctionValue),
    Struct(StructValue),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionValue {
    pub name: String,
    pub parameters: Vec<FunctionValueParameter>,
    pub body: Block,
    pub return_type: Box<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionValueParameter {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructValue {
    pub name: String,
    pub fields: Vec<StructValueField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructValueField {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Boolean,
    String,
    Array(Box<Type>),
    Function(FunctionType),
    Struct(StructType),
    // Pointer, Void
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub name: String,
    pub parameters: FunctionParametersType,
    pub return_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParametersType(pub Vec<Type>);

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<StructTypeField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructTypeField {
    pub name: String,
    pub ty: Type,
}

impl From<AstTypeKind> for Type {
    fn from(ty: AstTypeKind) -> Self {
        match ty {
            AstTypeKind::Int => Type::Int,
            AstTypeKind::Float => Type::Float,
            AstTypeKind::Boolean => Type::Boolean,
            AstTypeKind::String => Type::String,
            AstTypeKind::Array(ty) => Type::Array(Box::new(ty.ty.clone().kind.into())),
            // AstTypeKind::Function(name, parameters, return_type) => Type::Function(FunctionType {
            //     name,
            //     parameters: parameters.into(),
            //     return_type: Box::new(return_type.into()),
            // }),
            _ => unimplemented!(),
        }
    }
}

impl From<AstType> for Type {
    fn from(ty: AstType) -> Self {
        ty.kind.into()
    }
}

impl From<Value> for Type {
    fn from(value: Value) -> Self {
        match value {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Boolean(_) => Type::Boolean,
            Value::String(_) => Type::String,
            Value::Array(_) => unimplemented!(),
            Value::Function(function) => Type::Function(FunctionType {
                name: function.name,
                parameters: FunctionParametersType(
                    function.parameters.iter().map(|p| p.ty.clone()).collect(),
                ),
                return_type: function.return_type.clone(),
            }),
            Value::Struct(struct_value) => Type::Struct(StructType {
                name: struct_value.name,
                fields: struct_value
                    .fields
                    .iter()
                    .map(|f| StructTypeField {
                        name: f.name.clone(),
                        ty: f.ty.clone(),
                    })
                    .collect(),
            }),
        }
    }
}
