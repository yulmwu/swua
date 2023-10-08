use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    AddressSpace,
};

#[derive(Debug, Clone)]
pub enum Ty {
    Int,
    Float,
    String,
    Boolean,
    Array(Box<Ty>),
}

impl Ty {
    pub fn to_llvm_type_meta<'a>(&self, context: &'a Context) -> BasicMetadataTypeEnum<'a> {
        match self {
            Ty::Int => BasicMetadataTypeEnum::IntType(context.i64_type()),
            Ty::Float => BasicMetadataTypeEnum::FloatType(context.f64_type()),
            Ty::String => BasicMetadataTypeEnum::PointerType(
                context.i8_type().ptr_type(AddressSpace::from(0)),
            ),
            Ty::Boolean => BasicMetadataTypeEnum::IntType(context.bool_type()),
            Ty::Array(ty) => {
                BasicMetadataTypeEnum::ArrayType(ty.to_llvm_type_meta(context).into_array_type())
            }
        }
    }

    pub fn to_llvm_type<'a>(&self, context: &'a Context) -> BasicTypeEnum<'a> {
        match self {
            Ty::Int => context.i64_type().into(),
            Ty::Float => context.f64_type().into(),
            Ty::String => context.i8_type().ptr_type(AddressSpace::from(0)).into(),
            Ty::Boolean => context.bool_type().into(),
            Ty::Array(ty) => ty.to_llvm_type(context).array_type(0).into(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statements {
    Expression(Expression),
    Declaration(Declaration),
    Return(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    Identifier(IdentifierExpression),
    Call(CallExpression),
    Infix(InfixExpression),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

#[derive(Debug, Clone)]
pub enum LiteralExpression {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Expression>, Ty),
}

#[derive(Debug, Clone)]
pub struct IdentifierExpression {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<(String, Ty)>,
    pub body: Vec<Statements>,
}

pub mod ast_helper {
    use crate::*;

    #[inline(always)]
    pub fn var(name: &str) -> Statements {
        Statements::Declaration(Declaration::Variable(VariableDeclaration {
            name: name.to_string(),
            initializer: None,
        }))
    }

    #[inline(always)]
    pub fn var_init(name: &str, init: Expression) -> Statements {
        Statements::Declaration(Declaration::Variable(VariableDeclaration {
            name: name.to_string(),
            initializer: Some(init),
        }))
    }

    #[inline(always)]
    pub fn func(name: &str, parameters: Vec<(String, Ty)>, body: Vec<Statements>) -> Statements {
        Statements::Declaration(Declaration::Function(FunctionDeclaration {
            name: name.to_string(),
            parameters,
            body,
        }))
    }

    #[inline(always)]
    pub fn expr(expr: Expression) -> Statements {
        Statements::Expression(expr)
    }

    #[inline(always)]
    pub fn ret(expr: Expression) -> Statements {
        Statements::Return(expr)
    }

    #[inline(always)]
    pub fn lit(lit: LiteralExpression) -> Expression {
        Expression::Literal(lit)
    }

    #[inline(always)]
    pub fn ident(name: &str) -> Expression {
        Expression::Identifier(IdentifierExpression {
            name: name.to_string(),
        })
    }

    #[inline(always)]
    pub fn call(callee: Expression, arguments: Vec<Expression>) -> Expression {
        Expression::Call(CallExpression {
            callee: Box::new(callee),
            arguments,
        })
    }

    #[inline(always)]
    pub fn infix(operator: &str, left: Expression, right: Expression) -> Expression {
        Expression::Infix(InfixExpression {
            operator: operator.to_string(),
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    #[inline(always)]
    pub fn int(i: i64) -> LiteralExpression {
        LiteralExpression::Integer(i)
    }

    #[inline(always)]
    pub fn float(f: f64) -> LiteralExpression {
        LiteralExpression::Float(f)
    }

    #[inline(always)]
    pub fn string(s: &str) -> LiteralExpression {
        LiteralExpression::String(s.to_string())
    }

    #[inline(always)]
    pub fn boolean(b: bool) -> LiteralExpression {
        LiteralExpression::Boolean(b)
    }

    #[inline(always)]
    pub fn array(elements: Vec<Expression>, ty: Ty) -> LiteralExpression {
        LiteralExpression::Array(elements, ty)
    }
}
