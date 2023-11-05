use super::{CompileError, CompileResult, Identifier, Literal, Statement};
use crate::{
    BinaryOperator, CodegenType, Compiler, ExpressionCodegen, Position, StatementCodegen,
    SymbolTable, UnaryOperator, Value,
};
use inkwell::values::BasicMetadataValueEnum;

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Assign(AssignExpression),
    Block(BlockExpression),
    If(IfExpression),
    Call(CallExpression),
    Index(IndexExpression),
    Typeof(TypeofExpression),
    Sizeof(SizeofExpression),
}

impl ExpressionCodegen for Expression {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        macro_rules! inner {
            ($($ident:ident)*) => {
                match self {
                    $(
                        Expression::$ident(expression) => expression.codegen(compiler),
                    )*
                }
            };
        }
        inner! { Literal Binary Unary Assign Block If Call Index Typeof Sizeof }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
    pub position: Position,
}

impl ExpressionCodegen for BinaryExpression {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        use BinaryOperator::*;
        match self.operator {
            Dot => self.codegen_dot(compiler),
            Plus | Minus | Asterisk | Slash | Percent => self.codegen_arithmetic(compiler),
            EQ | NEQ | LT | GT | LTE | GTE => todo!(),
        }
    }
}

impl BinaryExpression {
    fn codegen_dot<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let left = self.left.codegen(compiler)?;
        let left_ty = match left.ty {
            CodegenType::StructType(struct_type) => struct_type,
            _ => return Err(CompileError::expected("struct", self.position)),
        };

        let right = match *self.right.clone() {
            Expression::Literal(Literal::Identifier(identifier)) => identifier,
            _ => return Err(CompileError::expected("identifier", self.position)),
        };

        let field = match left_ty.fields.get(&right.identifier) {
            Some(field) => field,
            None => {
                return Err(CompileError::field_not_found(
                    right.identifier,
                    right.position,
                ))
            }
        };
        let field_ll_ty = field.1.to_llvm_type(compiler.context);

        let ptr = unsafe {
            compiler.builder.build_gep(
                CodegenType::StructType(left_ty.clone()).to_llvm_type(compiler.context),
                left.llvm_value.into_pointer_value(),
                &[compiler.context.i64_type().const_int(field.0 as u64, false)],
                format!("ptr.struct.{}.{}", left_ty.name, field.0).as_str(),
            )
        };

        let load = compiler.builder.build_load(
            field_ll_ty,
            ptr,
            format!("struct.{}.{}", left_ty.name, field.0).as_str(),
        );

        Ok(Value::new(load, field.1.clone()))
    }

    fn codegen_arithmetic<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let left = self.left.codegen(compiler)?;
        let right = self.right.codegen(compiler)?;

        let left = match left.ty {
            CodegenType::Int => left.llvm_value.into_int_value(),
            _ => return Err(CompileError::expected("int", self.position)),
        };

        let right = match right.ty {
            CodegenType::Int => right.llvm_value.into_int_value(),
            _ => return Err(CompileError::expected("int", self.position)),
        };

        let result = match self.operator {
            BinaryOperator::Plus => compiler.builder.build_int_add(left, right, "add"),
            BinaryOperator::Minus => compiler.builder.build_int_sub(left, right, "sub"),
            BinaryOperator::Asterisk => compiler.builder.build_int_mul(left, right, "mul"),
            BinaryOperator::Slash => compiler.builder.build_int_signed_div(left, right, "div"),
            BinaryOperator::Percent => compiler.builder.build_int_signed_rem(left, right, "rem"),
            _ => unreachable!(),
        };

        Ok(Value::new(result.into(), CodegenType::Int))
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub expression: Box<Expression>,
    pub position: Position,
}

impl ExpressionCodegen for UnaryExpression {
    fn codegen<'a>(&self, _: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct AssignExpression {
    pub identifier: Identifier,
    pub value: Box<Expression>,
    pub position: Position,
}

impl ExpressionCodegen for AssignExpression {
    fn codegen<'a>(&self, _: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    pub statements: Vec<Statement>,
    pub position: Position,
}

impl ExpressionCodegen for BlockExpression {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let original_symbol_table = compiler.symbol_table.clone();
        compiler.symbol_table = SymbolTable::new_with_parent(compiler.symbol_table.clone());

        for statement in self.statements.clone() {
            if let Statement::Return(return_statement) = statement {
                let value = return_statement.value.codegen(compiler)?;
                compiler.builder.build_return(Some(&value.llvm_value));
                compiler.symbol_table = original_symbol_table;
                return Ok(value);
            }

            statement.codegen(compiler)?;
        }

        compiler.symbol_table = original_symbol_table;

        Ok(Value::new(
            compiler.context.i64_type().const_int(0, false).into(),
            CodegenType::Int, // Void
        ))
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockExpression,
    pub alternative: Option<BlockExpression>,
    pub position: Position,
}

impl ExpressionCodegen for IfExpression {
    fn codegen<'a>(&self, _: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Argument>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub value: Expression,
    pub position: Position,
}

impl ExpressionCodegen for CallExpression {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let (function, entry) = match *self.function.clone() {
            Expression::Literal(Literal::Identifier(identifier)) => {
                let function = match compiler.symbol_table.get_function(&identifier.identifier) {
                    Some(entry) => entry,
                    None => {
                        return Err(CompileError::function_not_found(
                            identifier.identifier,
                            identifier.position,
                        ))
                    }
                };
                let value = match compiler.module.get_function(&identifier.identifier) {
                    Some(value) => value,
                    None => {
                        return Err(CompileError::function_not_found(
                            identifier.identifier,
                            identifier.position,
                        ))
                    }
                };

                (value, function)
            }
            _ => return Err(CompileError::expected("identifier", self.position)),
        };

        let mut arguments: Vec<BasicMetadataValueEnum> = Vec::new();

        for argument in self.arguments.clone() {
            arguments.push(argument.value.codegen(compiler)?.llvm_value.into());
        }

        Ok(
            match compiler
                .builder
                .build_call(function, arguments.as_slice(), "call")
                .try_as_basic_value()
                .left()
            {
                Some(value) => Value::new(value, *entry.1.return_type.clone()),
                None => Value::new(
                    compiler.context.i64_type().const_int(0, false).into(),
                    CodegenType::Int, // Void
                ),
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
    pub position: Position,
}

impl ExpressionCodegen for IndexExpression {
    fn codegen<'a>(&self, _: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct TypeofExpression {
    pub expression: Box<Expression>,
    pub position: Position,
}

impl ExpressionCodegen for TypeofExpression {
    fn codegen<'a>(&self, _: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct SizeofExpression {
    pub expression: Box<Expression>,
    pub position: Position,
}

impl ExpressionCodegen for SizeofExpression {
    fn codegen<'a>(&self, _: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        todo!()
    }
}
