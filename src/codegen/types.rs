use crate::{
    codegen::{symbol_table::SymbolTable, CompileError, CompileResult, Identifier},
    Span,
};
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    values::IntValue,
    AddressSpace,
};
use std::{
    collections::BTreeMap,
    fmt,
    hash::{Hash, Hasher},
};

#[derive(Debug, PartialEq, Clone)]
pub struct AstType {
    pub kind: AstTypeKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstTypeKind {
    Int,
    Float,
    Boolean,
    String,
    Array(AstArrayTypeKind),
    TypeAlias(Identifier),
    Struct(Identifier),
    Void,
    Pointer(Box<AstType>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstArrayTypeKind {
    pub ty: Box<AstType>,
    pub len: Option<usize>,
    pub span: Span,
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
                span: array_type.span,
            }),
            AstTypeKind::TypeAlias(name) => match symbol_table.get_type_alias(&name.identifier) {
                Some(ty) => ty.ty,
                None => {
                    return Err(CompileError::type_not_found(
                        name.identifier.clone(),
                        name.span,
                    ))
                }
            },
            AstTypeKind::Struct(name) => match symbol_table.get_struct(&name.identifier) {
                Some(struct_type) => CodegenType::Struct(struct_type.struct_type),
                None => {
                    return Err(CompileError::struct_not_found(
                        name.identifier.clone(),
                        name.span,
                    ))
                }
            },
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
            AstTypeKind::String => write!(f, "str"),
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
            AstTypeKind::TypeAlias(name) => write!(f, "@{}", name.identifier),
            AstTypeKind::Struct(name) => write!(f, "{}", name.identifier),
            AstTypeKind::Pointer(ty) => write!(f, "{}*", ty.kind),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
pub struct ArrayType {
    pub ty: Box<CodegenType>,
    pub len: Option<usize>,
    pub span: Span,
}

impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Hash for ArrayType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        4.hash(state);
        self.ty.hash(state);
    }
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct StructType {
    pub name: String,
    pub fields: BTreeMap<String, (usize, CodegenType)>,
    pub span: Span,
}

impl Hash for StructType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        5.hash(state);
        self.name.hash(state);
        for (name, (_, ty)) in &self.fields {
            name.hash(state);
            ty.hash(state);
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct FunctionType {
    pub name: String,
    pub parameters: FunctionParametersType,
    pub return_type: Box<CodegenType>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct FunctionParametersType(pub Vec<CodegenType>);

impl From<Vec<CodegenType>> for FunctionParametersType {
    fn from(parameters: Vec<CodegenType>) -> Self {
        Self(parameters)
    }
}

impl Hash for FunctionParametersType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for ty in &self.0 {
            ty.hash(state);
        }
    }
}

impl Hash for FunctionType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        6.hash(state);
        self.name.hash(state);
        self.parameters.hash(state);
        // self.return_type.hash(state);
    }
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
                    .0
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
            CodegenType::Void => context.i8_type().ptr_type(AddressSpace::from(0)).into(),
        }
    }

    pub fn size_of<'a>(&self, context: &'a Context, span: Span) -> CompileResult<IntValue<'a>> {
        Ok(match self.to_llvm_type(context).size_of() {
            Some(size) => size,
            None => return Err(CompileError::unknown_size(span)),
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
                write!(f, "fn {}", function_type.name)?;
                if !function_type.parameters.0.is_empty() {
                    write!(f, "(")?;
                    for (i, ty) in function_type.parameters.0.iter().enumerate() {
                        write!(f, "{}", ty)?;
                        if i < function_type.parameters.0.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")?;
                }
                write!(f, " -> {}", function_type.return_type)
            }
            CodegenType::Void => write!(f, "void"),
            CodegenType::Pointer(ty) => write!(f, "{}*", ty),
        }
    }
}

impl Hash for CodegenType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            CodegenType::Int => 0.hash(state),
            CodegenType::Float => 1.hash(state),
            CodegenType::Boolean => 2.hash(state),
            CodegenType::String => 3.hash(state),
            CodegenType::Array(arr) => arr.hash(state),
            CodegenType::Struct(struct_type) => struct_type.hash(state),
            CodegenType::Function(function_type) => function_type.hash(state),
            CodegenType::Void => 7.hash(state),
            CodegenType::Pointer(ty) => {
                8.hash(state);
                ty.hash(state);
            }
        }
    }
}
