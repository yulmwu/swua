use super::{BlockExpression, Expression, Position, Ty};
use std::collections::HashMap;

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

            impl From<$ty> for $name {
                fn from(value: $ty) -> Self {
                    Self {
                        value,
                        position: Position::default(),
                    }
                }
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
    pub elements: Vec<(Expression, Position)>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLiteral {
    pub identifier: Identifier,
    pub fields: HashMap<String, (Expression, Position)>,
    pub position: Position,
}
