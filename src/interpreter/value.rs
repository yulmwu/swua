use crate::{
    codegen::{
        types::{AstType, AstTypeKind},
        Block,
    },
    Span,
};

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
    Function(FunctionValue),
}

#[derive(Clone, Debug)]
pub struct FunctionValue {
    pub name: String,
    pub parameters: Vec<FunctionValueParameter>,
    pub body: Block,
    pub return_type: Box<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionValueParameter {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    Boolean,
    String,
    Array(Box<Type>),
    Function(FunctionType),
    // Pointer, Void, Struct
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub name: String,
    pub parameters: FunctionParametersType,
    pub return_type: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct FunctionParametersType(pub Vec<Type>);

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
