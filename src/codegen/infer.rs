use super::{
    error::{CompileError, CompileResult},
    SymbolTable,
};
use crate::ast::{
    expression::Expression,
    literal::{ArrayLiteral, FunctionLiteral, Literal, StructLiteral},
    FunctionType, StructField, StructType, Ty, TyKind,
};

pub fn infer_expression(
    expression: Expression,
    symbol_table: &SymbolTable,
) -> CompileResult<TyKind> {
    Ok(match expression {
        Expression::AssignmentExpression(_) => todo!(),
        Expression::BlockExpression(_) => todo!(),
        Expression::PrefixExpression(_) => todo!(),
        Expression::InfixExpression(_) => todo!(),
        Expression::IfExpression(_) => todo!(),
        Expression::CallExpression(_) => todo!(),
        Expression::TypeofExpression(_) => todo!(),
        Expression::IndexExpression(index_expression) => {
            let array_ty = infer_expression(*index_expression.left, symbol_table)?;
            match array_ty {
                TyKind::Array(ty) => ty.kind.clone(),
                _ => {
                    return Err(CompileError::indexing_non_array_type(
                        index_expression.position,
                    ))
                }
            }
        }
        Expression::Literal(literal) => infer_literal(literal, symbol_table)?,
        Expression::Debug(_, _) => todo!(),
    })
}

pub fn infer_literal(literal: Literal, symbol_table: &SymbolTable) -> CompileResult<TyKind> {
    Ok(match literal {
        Literal::Int(_) => TyKind::Int,
        Literal::Float(_) => TyKind::Float,
        Literal::String(_) => TyKind::String,
        Literal::Boolean(_) => TyKind::Boolean,
        Literal::Array(ArrayLiteral { elements, position }) => {
            let mut ty: Option<Ty> = None;
            for element in elements {
                match ty {
                    Some(ref ty) => {
                        if ty.kind != infer_expression(element, symbol_table)? {
                            return Err(CompileError::array_elements_must_be_of_the_same_type(
                                position,
                            ));
                        }
                    }
                    None => {
                        ty = Some(Ty::new(infer_expression(element, symbol_table)?, position));
                        continue;
                    }
                }
            }
            match ty {
                Some(ty) => TyKind::Array(Box::new(ty)),
                None => return Err(CompileError::array_must_have_at_least_one_element(position)),
            }
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
                    ty: Ty::new(infer_expression(expression, symbol_table)?, position),
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
        Literal::Identifier(identifier) => match symbol_table.variables.get(&identifier.value) {
            Some(ty) => ty.1.clone(),
            None => {
                return Err(CompileError::identifier_not_found(
                    identifier.value,
                    identifier.position,
                ))
            }
        },
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
    })
}
