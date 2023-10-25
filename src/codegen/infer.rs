use super::{
    error::{CompileError, CompileResult},
    symbol_table::SymbolTable,
};
use crate::ast::{
    expression::{
        BlockExpression, CallExpression, Expression, IfExpression, IndexExpression,
        InfixExpression, InfixOperator,
    },
    literal::{ArrayLiteral, FunctionLiteral, Literal, StructLiteral},
    statement::Statement,
    FunctionType, StructField, StructType, Ty, TyKind,
};

pub fn infer_expression(
    expression: Expression,
    symbol_table: &mut SymbolTable,
) -> CompileResult<TyKind> {
    Ok(match expression {
        Expression::AssignmentExpression(_) => todo!(),
        Expression::BlockExpression(BlockExpression { statements, .. }) => {
            let mut ty = TyKind::Void;
            for statement in statements {
                if let Statement::ReturnStatement(stmt) = statement {
                    ty = infer_expression(stmt.value, symbol_table)?;
                }
            }
            ty
        }
        Expression::PrefixExpression(_) => todo!(),
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

                    if left_ty == TyKind::Int || left_ty == TyKind::Float {
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
                        if function_ty.parameters[i].kind
                            != infer_expression(argument.clone(), symbol_table)?
                        {
                            return Err(CompileError::type_mismatch(
                                function_ty.parameters[i].kind.clone(),
                                infer_expression(argument.clone(), symbol_table)?,
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
        Expression::TypeofExpression(_) => todo!(),
        Expression::IndexExpression(IndexExpression { left, position, .. }) => {
            let array_ty = infer_expression(*left, symbol_table)?;
            match array_ty {
                TyKind::Array(ty) => ty.kind.clone(),
                _ => return Err(CompileError::indexing_non_array_type(position)),
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
