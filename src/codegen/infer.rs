use super::SymbolTable;
use crate::ast::{
    expression::Expression,
    literal::{ArrayLiteral, FunctionLiteral, Literal, StructLiteral},
    FunctionType, StructField, StructType, Ty, TyKind,
};

pub fn infer_expression(expression: Expression, symbol_table: &SymbolTable) -> TyKind {
    match expression {
        Expression::AssignmentExpression(_) => todo!(),
        Expression::BlockExpression(_) => todo!(),
        Expression::PrefixExpression(_) => todo!(),
        Expression::InfixExpression(_) => todo!(),
        Expression::IfExpression(_) => todo!(),
        Expression::CallExpression(_) => todo!(),
        Expression::TypeofExpression(_) => todo!(),
        Expression::IndexExpression(index_expression) => {
            let array_ty = infer_expression(*index_expression.left, symbol_table);
            match array_ty {
                TyKind::Array(ty) => ty.kind.clone(),
                _ => panic!("Indexing a non-array type"),
            }
        }
        Expression::Literal(literal) => infer_literal(literal, symbol_table),
        Expression::Debug(_, _) => todo!(),
    }
}

pub fn infer_literal(literal: Literal, symbol_table: &SymbolTable) -> TyKind {
    match literal {
        Literal::Int(_) => TyKind::Int,
        Literal::Float(_) => TyKind::Float,
        Literal::String(_) => TyKind::String,
        Literal::Boolean(_) => TyKind::Boolean,
        Literal::Array(ArrayLiteral { elements, position }) => {
            let mut ty = None;
            for element in elements {
                if ty.is_none() {
                    ty = Some(Ty::new(infer_expression(element, symbol_table), position));
                    continue;
                }

                assert_eq!(
                    ty.clone().unwrap().kind,
                    infer_expression(element, symbol_table),
                    "Array elements must be of the same type"
                );
            }
            TyKind::Array(Box::new(ty.expect("Array must have at least one element")))
        }
        Literal::Struct(StructLiteral {
            identifier,
            fields,
            position,
        }) => {
            let mut fields_ty = Vec::new();
            for (identifier, expression) in fields {
                fields_ty.push(StructField {
                    identifier,
                    ty: Ty::new(infer_expression(expression, symbol_table), position),
                    position: Default::default(),
                });
            }
            TyKind::Struct(StructType {
                identifier,
                generics: None,
                fields: fields_ty,
                position,
            })
        }
        Literal::Identifier(identifier) => {
            symbol_table
                .variables
                .get(&identifier.value)
                .unwrap()
                .clone()
                .1
        }
        Literal::Function(FunctionLiteral {
            parameters,
            body: _,
            generics,
            ret,
            position,
        }) => {
            let mut parameters_ty = Vec::new();
            for parameter in parameters {
                parameters_ty.push(parameter.ty);
            }
            TyKind::Fn(FunctionType {
                generics,
                parameters: parameters_ty,
                ret: Box::new(ret),
                position,
            })
        }
    }
}
