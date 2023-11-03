use std::collections::HashMap;

use super::{
    error::{CompileError, CompileResult},
    symbol_table::{SymbolTable, VariableEntry},
};
use crate::ast::{
    expression::{
        AssignmentExpression, BlockExpression, CallExpression, Expression, IfExpression,
        IndexExpression, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator,
    },
    literal::{ArrayLiteral, FunctionLiteral, Literal, StructLiteral},
    statement::Statement,
    ArrayType, FunctionType, StructType, Ty, TyKind,
};

pub fn infer_expression(
    expression: Expression,
    symbol_table: &mut SymbolTable,
) -> CompileResult<TyKind> {
    Ok(match expression {
        Expression::AssignmentExpression(AssignmentExpression {
            identifier,
            value,
            position,
        }) => {
            let value_ty = infer_expression(*value, symbol_table)?;
            if let Some(VariableEntry { ty, .. }) = symbol_table.get_variable(&identifier.value) {
                if value_ty != *ty {
                    return Err(CompileError::type_mismatch(&value_ty, ty, position));
                }
            } else {
                return Err(CompileError::identifier_not_found(
                    identifier.value,
                    identifier.position,
                ));
            }
            value_ty
        }
        Expression::BlockExpression(BlockExpression { statements, .. }) => {
            let mut ty = TyKind::Void;
            for statement in statements {
                if let Statement::ReturnStatement(stmt) = statement {
                    ty = infer_expression(stmt.value, symbol_table)?;
                }
            }
            ty
        }
        Expression::PrefixExpression(PrefixExpression {
            operator,
            right,
            position,
        }) => {
            let right_ty = infer_expression(*right, symbol_table)?;
            use PrefixOperator::*;

            match operator {
                Minus => {
                    if right_ty == TyKind::Int || right_ty == TyKind::Float {
                        right_ty
                    } else {
                        return Err(CompileError::type_mismatch(
                            "Int or Float".to_string(),
                            right_ty.to_string(),
                            position,
                        ));
                    }
                }
                Not => {
                    if right_ty == TyKind::Boolean {
                        right_ty
                    } else {
                        return Err(CompileError::type_mismatch(
                            TyKind::Boolean.to_string(),
                            right_ty.to_string(),
                            position,
                        ));
                    }
                }
            }
        }
        Expression::InfixExpression(InfixExpression {
            left,
            right,
            operator,
            position,
        }) => {
            let left_ty = infer_expression(*left, symbol_table)?;
            let right_ty = infer_expression(*right, symbol_table)?;
            use InfixOperator::*;

            match operator {
                Plus | Minus | Asterisk | Slash | Percent => {
                    if left_ty != right_ty {
                        return Err(CompileError::type_mismatch(left_ty, right_ty, position));
                    }

                    if left_ty == TyKind::Int
                        || left_ty == TyKind::Float
                        || (left_ty == TyKind::String && operator == Plus)
                    {
                        left_ty
                    } else {
                        return Err(CompileError::type_mismatch(
                            "Int or Float".to_string(),
                            left_ty.to_string(),
                            position,
                        ));
                    }
                }
                EQ | NEQ => {
                    if left_ty != right_ty {
                        return Err(CompileError::type_mismatch(left_ty, right_ty, position));
                    }
                    TyKind::Boolean
                }
                _ => todo!(),
            }
        }
        Expression::IfExpression(IfExpression {
            consequence,
            alternative,
            position,
            ..
        }) => {
            let ty = infer_expression(Expression::BlockExpression(*consequence), symbol_table)?;
            if let Some(alternative) = alternative {
                if ty
                    != infer_expression(
                        Expression::BlockExpression(*alternative.clone()),
                        symbol_table,
                    )?
                {
                    return Err(CompileError::if_else_must_have_the_same_type(position));
                }
            }
            ty
        }
        Expression::CallExpression(CallExpression {
            function,
            arguments,
            position,
        }) => {
            let function_ty = infer_expression(*function, symbol_table)?;
            match function_ty {
                TyKind::Fn(function_ty) => {
                    if function_ty.parameters.len() != arguments.len() {
                        return Err(CompileError::wrong_number_of_arguments(
                            function_ty.parameters.len(),
                            arguments.len(),
                            function_ty.position,
                        ));
                    }
                    for (i, argument) in arguments.iter().enumerate() {
                        let parameter_ty = function_ty.parameters[i].kind.clone();
                        if parameter_ty != infer_expression(argument.0.clone(), symbol_table)? {
                            return Err(CompileError::type_mismatch(
                                parameter_ty,
                                infer_expression(argument.0.clone(), symbol_table)?,
                                function_ty.position,
                            ));
                        }
                    }
                    function_ty.ret.kind
                }
                _ => {
                    return Err(CompileError::call_non_function_type(
                        function_ty.to_string(),
                        position,
                    ))
                }
            }
        }
        Expression::TypeofExpression(_)
        | Expression::SizeofExpression(_)
        | Expression::SizeofTypeExpression(_) => TyKind::Int,
        Expression::IndexExpression(IndexExpression {
            left,
            index,
            position,
            ..
        }) => {
            let left = infer_expression(*left, symbol_table)?;
            match left {
                TyKind::Array(array_type) => array_type.element_ty.kind.clone(),
                TyKind::Struct(struct_type) => match *index {
                    Expression::Literal(Literal::String(ident)) => {
                        struct_type.fields.get(&ident.value).unwrap().1.kind.clone()
                    }
                    _ => return Err(CompileError::expected("String", position)),
                },
                _ => return Err(CompileError::type_that_cannot_indexed(position)),
            }
        }
        Expression::Literal(literal) => infer_literal(literal, symbol_table)?,
        Expression::Debug(_, _) => todo!(),
    })
}

pub fn infer_literal(literal: Literal, symbol_table: &mut SymbolTable) -> CompileResult<TyKind> {
    Ok(match literal {
        Literal::Int(_) => TyKind::Int,
        Literal::Float(_) => TyKind::Float,
        Literal::String(_) => TyKind::String,
        Literal::Boolean(_) => TyKind::Boolean,
        Literal::Array(ArrayLiteral { elements, position }) => {
            let mut ty: Option<Ty> = None;
            for (element, position) in elements.clone() {
                match ty {
                    Some(ref ty) => {
                        let infered_ty = infer_expression(element, symbol_table)?;
                        if ty.kind != infered_ty {
                            println!("{:#?} {:#? }", ty, infered_ty);
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
                Some(ty) => TyKind::Array(ArrayType {
                    element_ty: Box::new(ty),
                    size: Some(elements.len()),
                    position,
                }),
                None => return Err(CompileError::array_must_have_at_least_one_element(position)),
            }
        }
        Literal::Struct(StructLiteral {
            identifier,
            fields,
            position,
        }) => {
            let mut fields_ty = HashMap::new();
            for (index, (identifier, expression)) in fields.iter().enumerate() {
                fields_ty.insert(
                    identifier.clone(),
                    (
                        index,
                        Ty::new(
                            infer_expression(expression.clone().0, symbol_table)?,
                            expression.1,
                        ),
                    ),
                );
            }
            TyKind::Struct(StructType {
                identifier: identifier.value,
                fields: fields_ty,
                position,
            })
        }
        Literal::Identifier(identifier) => match symbol_table.get_variable(&identifier.value) {
            Some(ty) => ty.ty.clone(),
            None => match symbol_table.get_function(&identifier.value) {
                Some(ty) => TyKind::Fn(ty.ty.clone()),
                None => {
                    return Err(CompileError::identifier_not_found(
                        identifier.value,
                        identifier.position,
                    ))
                }
            },
        },
        Literal::Function(FunctionLiteral {
            parameters,
            ret,
            position,
            ..
        }) => {
            let mut parameters_ty = Vec::new();
            for parameter in parameters {
                parameters_ty.push(parameter.ty);
            }
            TyKind::Fn(FunctionType {
                parameters: parameters_ty,
                ret: Box::new(ret),
                position,
            })
        }
    })
}
