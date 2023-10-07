use inkwell::{context::Context, types::BasicMetadataTypeEnum, AddressSpace};

pub enum Ty {
    Int,
    Float,
    String,
    Boolean,
}

impl Ty {
    pub fn to_llvm_type<'a>(&self, context: &'a Context) -> BasicMetadataTypeEnum<'a> {
        match self {
            Ty::Int => BasicMetadataTypeEnum::IntType(context.i64_type()),
            Ty::Float => BasicMetadataTypeEnum::FloatType(context.f64_type()),
            Ty::String => BasicMetadataTypeEnum::PointerType(
                context.i8_type().ptr_type(AddressSpace::from(0)),
            ),
            Ty::Boolean => BasicMetadataTypeEnum::IntType(context.bool_type()),
        }
    }
}

pub enum Statements {
    Expression(Expression),
    Declaration(Declaration),
    Return(Expression),
}

pub enum Expression {
    Literal(LiteralExpression),
    Identifier(IdentifierExpression),
    Call(CallExpression),
    Infix(InfixExpression),
}

pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

pub enum LiteralExpression {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

pub struct IdentifierExpression {
    pub name: String,
}

pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

pub struct InfixExpression {
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub struct VariableDeclaration {
    pub name: String,
    pub initializer: Option<Expression>,
}

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
}
