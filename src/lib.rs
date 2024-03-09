pub mod cli;
pub mod codegen;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod preprocessor;
pub mod utils;

use codegen::{
    symbol_table::SymbolTable,
    types::{CodegenType, FunctionType, StructType},
    CompileResult, Statement,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::TargetTriple,
    values::{BasicValueEnum, FunctionValue},
};
use lexer::tokens::TokenKind;
use std::fmt;

#[derive(Debug)]
pub struct Compiler<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    pub current_function: Option<CurrentFunction<'a>>,
    pub current_return: Option<Value<'a>>,
    pub symbol_table: SymbolTable<'a>,
}

#[derive(Debug, Clone)]
pub struct CurrentFunction<'a> {
    pub function: FunctionValue<'a>,
    pub return_type: CodegenType,
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

#[derive(Debug, Default, Clone, Copy, Eq, PartialOrd, Ord)]
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

#[derive(Debug, Clone, Copy, Default, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl PartialEq for Span {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl From<Position> for Span {
    fn from(position: Position) -> Self {
        Self {
            start: position,
            end: position,
        }
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
        triple: &TargetTriple,
        name: &str,
    ) -> CompileResult<Module<'a>> {
        let module = context.create_module(name);
        module.set_triple(triple);
        let builder = context.create_builder();

        let mut compiler = Compiler {
            context,
            module,
            builder,
            current_function: None,
            current_return: None,
            symbol_table,
        };

        for statement in self.statements.clone() {
            statement.codegen(&mut compiler)?;
        }

        Ok(compiler.module)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for statement in self.statements.iter() {
            statement.display(f, 0)?;
        }
        Ok(())
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

impl From<TokenKind> for BinaryOperator {
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

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dot => write!(f, "."),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::EQ => write!(f, "=="),
            Self::NEQ => write!(f, "!="),
            Self::GT => write!(f, ">"),
            Self::GTE => write!(f, ">="),
            Self::LT => write!(f, "<"),
            Self::LTE => write!(f, "<="),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Minus,
    Not,
}

impl From<TokenKind> for UnaryOperator {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::Minus => Self::Minus,
            TokenKind::Bang => Self::Not,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum Priority {
    Lowest,
    Assign_,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
    Cast,
    MemberAccess,
    StructLiteral,
}

impl From<TokenKind> for Priority {
    fn from(token_kind: TokenKind) -> Self {
        use Priority::*;
        use TokenKind::*;
        match token_kind {
            Assign | Question => Assign_,
            EQ | NEQ => Equals,
            LT | GT | LTE | GTE => LessGreater,
            Plus | Minus => Sum,
            Slash | Asterisk | Percent => Product,
            Typeof | Sizeof => Prefix,
            LParen => Call,
            LBracket => Index,
            As => Cast,
            Dot => MemberAccess,
            LBrace => StructLiteral,
            _ => Lowest,
        }
    }
}

pub trait DisplayNode {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result;
}

pub mod display {
    use std::fmt;

    #[inline]
    pub fn indent(f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write!(f, "{}", " ".repeat(indent * 4))
    }
}
