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

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntLiteral {
    pub value: i64,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatLiteral {
    pub value: f64,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
    pub position: Position,
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
