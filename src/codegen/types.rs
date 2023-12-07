use crate::{
    codegen::{symbol_table::SymbolTable, CompileError, CompileResult, Identifier},
    Position,
};
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    values::IntValue,
    AddressSpace,
};
use std::{collections::BTreeMap, fmt};

#[derive(Debug, PartialEq, Clone)]
pub struct AstType {
    pub kind: AstTypeKind,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstTypeKind {
    Int,
    Float,
    Boolean,
    String,
    Array(AstArrayTypeKind),
    Named(Identifier),
    Void,
    Pointer(Box<AstType>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstArrayTypeKind {
    pub ty: Box<AstType>,
    pub len: Option<usize>,
    pub position: Position,
}

impl AstTypeKind {
    pub fn to_codegen_type(&self, symbol_table: &SymbolTable) -> CompileResult<CodegenType> {
        Ok(match self {
            AstTypeKind::Int => CodegenType::Int,
            AstTypeKind::Float => CodegenType::Float,
            AstTypeKind::Boolean => CodegenType::Boolean,
            AstTypeKind::String => CodegenType::String,
            AstTypeKind::Void => CodegenType::Void,
            AstTypeKind::Array(array_type) => CodegenType::Array(ArrayType {
                ty: Box::new(array_type.ty.kind.to_codegen_type(symbol_table)?),
                len: array_type.len,
                position: array_type.position,
            }),
            AstTypeKind::Named(name) => {
                let struct_type = match symbol_table.get_struct(&name.identifier) {
                    Some(struct_type) => struct_type,
                    None => {
                        return Err(CompileError::struct_not_found(
                            name.identifier.clone(),
                            name.position,
                        ))
                    }
                };
                CodegenType::Struct(struct_type.1.clone())
            }
            AstTypeKind::Pointer(ty) => {
                CodegenType::Pointer(Box::new(ty.kind.to_codegen_type(symbol_table)?))
            }
        })
    }
}

impl fmt::Display for AstTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstTypeKind::Int => write!(f, "int"),
            AstTypeKind::Float => write!(f, "float"),
            AstTypeKind::Boolean => write!(f, "boolean"),
            AstTypeKind::String => write!(f, "string"),
            AstTypeKind::Void => write!(f, "void"),
            AstTypeKind::Array(array_type) => write!(
                f,
                "{}[{}]",
                array_type.ty.kind,
                if let Some(len) = array_type.len {
                    len.to_string()
                } else {
                    String::new()
                }
            ),
            AstTypeKind::Named(name) => write!(f, "{}", name.identifier),
            AstTypeKind::Pointer(ty) => write!(f, "{}*", ty.kind),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CodegenType {
    Int,
    Float,
    Boolean,
    String,
    Array(ArrayType),
    Struct(StructType),
    Function(FunctionType),
    Void,
    Pointer(Box<CodegenType>),
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub ty: Box<CodegenType>,
    pub len: Option<usize>,
    pub position: Position,
}

impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub name: String,
    pub fields: BTreeMap<String, (usize, CodegenType)>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub name: String,
    pub parameters: Vec<CodegenType>,
    pub return_type: Box<CodegenType>,
    pub position: Position,
}

impl CodegenType {
    pub fn to_llvm_type<'a>(&self, context: &'a Context) -> BasicTypeEnum<'a> {
        match self {
            CodegenType::Int => context.i64_type().into(),
            CodegenType::Float => context.f64_type().into(),
            CodegenType::Boolean => context.bool_type().into(),
            CodegenType::String => context.i8_type().ptr_type(AddressSpace::from(0)).into(),
            CodegenType::Array(arr) => arr
                .ty
                .to_llvm_type(context)
                .ptr_type(AddressSpace::from(0))
                .into(),
            CodegenType::Struct(struct_type) => context
                .struct_type(
                    &struct_type
                        .fields
                        .iter()
                        .map(|(_, (_, ty))| ty.to_llvm_type(context))
                        .collect::<Vec<_>>(),
                    false,
                )
                .ptr_type(AddressSpace::from(0))
                .into(),
            CodegenType::Function(function_type) => {
                let parameters = function_type
                    .parameters
                    .iter()
                    .map(|ty| ty.to_llvm_type(context).into())
                    .collect::<Vec<_>>();
                let return_type = function_type.return_type.to_llvm_type(context);
                return_type
                    .fn_type(parameters.as_slice(), false)
                    .ptr_type(AddressSpace::from(0))
                    .into()
            }
            CodegenType::Pointer(ty) => ty
                .to_llvm_type(context)
                .ptr_type(AddressSpace::from(0))
                .into(),
            _ => unimplemented!(),
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

impl fmt::Display for CodegenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodegenType::Int => write!(f, "int"),
            CodegenType::Float => write!(f, "float"),
            CodegenType::Boolean => write!(f, "boolean"),
            CodegenType::String => write!(f, "string"),
            CodegenType::Array(arr) => write!(
                f,
                "{}[{}]",
                arr.ty,
                if let Some(len) = arr.len {
                    len.to_string()
                } else {
                    String::new()
                }
            ),
            CodegenType::Struct(struct_type) => write!(f, "struct {}", struct_type.name),
            CodegenType::Function(function_type) => {
                write!(f, "fn {}", function_type.name)
            }
            CodegenType::Void => write!(f, "void"),
            CodegenType::Pointer(ty) => write!(f, "{}*", ty),
        }
    }
}