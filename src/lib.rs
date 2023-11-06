pub mod codegen;
pub mod parser;
pub mod tokenizer;

use codegen::{CompileError, CompileResult, Identifier, Statement};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{self, BasicType, BasicTypeEnum},
    values::{BasicValueEnum, IntValue, PointerValue},
    AddressSpace,
};
use std::{collections::BTreeMap, fmt};
use tokenizer::TokenKind;

#[derive(Debug, Clone, Default)]
pub struct SymbolEntries<'a> {
    pub symbols: BTreeMap<String, (PointerValue<'a>, CodegenType)>,
    pub functions: BTreeMap<String, (types::FunctionType<'a>, FunctionType)>,
    pub structs: BTreeMap<String, (types::StructType<'a>, StructType)>,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    pub entries: SymbolEntries<'a>,
    pub parent: Option<Box<SymbolTable<'a>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with_parent(parent: SymbolTable<'a>) -> Self {
        Self {
            entries: SymbolEntries::default(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn insert_variable(&mut self, name: String, ty: CodegenType, value: PointerValue<'a>) {
        self.entries.symbols.insert(name, (value, ty));
    }

    pub fn insert_function(
        &mut self,
        name: String,
        ty: types::FunctionType<'a>,
        function_type: FunctionType,
    ) {
        self.entries.functions.insert(name, (ty, function_type));
    }

    pub fn insert_struct(
        &mut self,
        name: String,
        ty: types::StructType<'a>,
        struct_type: StructType,
    ) {
        self.entries.structs.insert(name, (ty, struct_type));
    }

    pub fn get_variable(&self, name: &str) -> Option<(PointerValue<'a>, CodegenType)> {
        match self.entries.symbols.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_variable(name),
                None => None,
            },
        }
    }

    pub fn get_function(&self, name: &str) -> Option<(types::FunctionType<'a>, FunctionType)> {
        match self.entries.functions.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_function(name),
                None => None,
            },
        }
    }

    pub fn get_struct(&self, name: &str) -> Option<(types::StructType<'a>, StructType)> {
        match self.entries.structs.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_struct(name),
                None => None,
            },
        }
    }
}

#[derive(Debug)]
pub struct Compiler<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    pub symbol_table: SymbolTable<'a>,
}

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
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstArrayTypeKind {
    pub ty: Box<AstType>,
    pub len: Option<usize>,
    pub position: Position,
}

impl AstTypeKind {
    fn to_codegen_type(&self, symbol_table: &SymbolTable) -> CompileResult<CodegenType> {
        Ok(match self {
            AstTypeKind::Int => CodegenType::Int,
            AstTypeKind::Float => CodegenType::Float,
            AstTypeKind::Boolean => CodegenType::Boolean,
            AstTypeKind::String => CodegenType::String,
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
        })
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
        }
    }
}

pub trait StatementCodegen: Clone {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()>;
}

pub trait ExpressionCodegen: Clone {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>>;
}

#[derive(Debug, Clone)]
pub struct Value<'a> {
    pub llvm_value: BasicValueEnum<'a>,
    pub ty: CodegenType,
}

impl<'a> Value<'a> {
    pub fn new(llvm_value: BasicValueEnum<'a>, ty: CodegenType) -> Self {
        Self { llvm_value, ty }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl PartialEq for Position {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

impl Program {
    pub fn codegen<'a>(
        &self,
        context: &'a Context,
        symbol_table: SymbolTable<'a>,
    ) -> CompileResult<Module<'a>> {
        let module = context.create_module("main");
        let builder = context.create_builder();

        let mut compiler = Compiler {
            context,
            module,
            builder,
            symbol_table,
        };

        for statement in self.statements.clone() {
            statement.codegen(&mut compiler)?;
        }

        Ok(compiler.module)
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Dot,      // A.B
    Plus,     // A + B
    Minus,    // A - B
    Asterisk, // A * B
    Slash,    // A / B
    Percent,  // A % B
    EQ,       // A == B
    NEQ,      // A != B
    GT,       // A > B
    GTE,      // A >= B
    LT,       // A < B
    LTE,      // A <= B
}

impl From<TokenKind<'_>> for BinaryOperator {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Dot => Self::Dot,
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Asterisk => Self::Asterisk,
            TokenKind::Slash => Self::Slash,
            TokenKind::Percent => Self::Percent,
            TokenKind::EQ => Self::EQ,
            TokenKind::NEQ => Self::NEQ,
            TokenKind::GT => Self::GT,
            TokenKind::GTE => Self::GTE,
            TokenKind::LT => Self::LT,
            TokenKind::LTE => Self::LTE,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Minus,
    Not,
}

impl From<TokenKind<'_>> for UnaryOperator {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Minus => Self::Minus,
            TokenKind::Bang => Self::Not,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum Priority {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
    MemberAccess,
}
