use super::{Expression, FunctionLiteral, Identifier, Position, Ty};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    FunctionDeclaration(FunctionDeclaration),
    ExternFunctionDeclaration(ExternFunctionDeclaration),
    ReturnStatement(ReturnStatement),
    TypeStatement(TypeStatement),
    DeclareStatement(DeclareStatement),
    StructDeclaration(StructDeclaration),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub identifier: Identifier,
    pub value: Option<Expression>,
    pub ty: Option<Ty>,
    pub is_mutable: bool,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub identifier: Identifier,
    pub function: FunctionLiteral,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExternFunctionDeclaration {
    pub identifier: Identifier,
    pub parameters: Vec<Ty>,
    pub ret: Ty,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeStatement {
    pub identifier: Identifier,
    pub ty: Ty,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclareStatement {
    pub identifier: Identifier,
    pub ty: Ty,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclaration {
    pub identifier: Identifier,
    pub fields: HashMap<String, Ty>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub value: Expression,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub position: Position,
}
