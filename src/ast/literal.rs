use super::{BlockExpression, Expression, IdentifierGeneric, Position, Ty};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Identifier(Identifier),
    Int(IntLiteral),
    Float(FloatLiteral),
    String(StringLiteral),
    Boolean(BooleanLiteral),
    Function(FunctionLiteral),
    Array(ArrayLiteral),
    Struct(StructLiteral),
}

macro_rules! scalar_type {
    ($($name:ident: $ty:ty),*) => {
        $(
            #[derive(Debug, PartialEq, Clone)]
            pub struct $name {
                pub value: $ty,
                pub position: Position,
            }
        )*
    }
}

scalar_type! {
    Identifier: String,
    IntLiteral: i64,
    FloatLiteral: f64,
    StringLiteral: String,
    BooleanLiteral: bool
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub parameters: Vec<Parameter>,
    pub body: BlockExpression,
    pub generics: Option<IdentifierGeneric>,
    pub ret: Ty,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    pub identifier: Identifier,
    pub ty: Ty,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
    pub ty: Ty,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLiteral {
    pub identifier: Identifier,
    pub fields: Vec<(Identifier, Expression)>,
    pub position: Position,
}
