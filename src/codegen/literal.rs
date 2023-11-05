use super::{CompileError, CompileResult, Expression};
use crate::{CodegenType, Compiler, ExpressionCodegen, Position, Value};
use inkwell::values::{BasicValue, BasicValueEnum};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum Literal {
    Identifier(Identifier),
    Int(IntLiteral),
    Float(FloatLiteral),
    Boolean(BooleanLiteral),
    String(StringLiteral),
    Array(ArrayLiteral),
    Struct(StructLiteral),
}

impl ExpressionCodegen for Literal {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        macro_rules! inner {
            ($($ident:ident)*) => {
                match self {
                    $(
                        Literal::$ident(literal) => literal.codegen(compiler),
                    )*
                }
            };
        }
        inner! { Identifier Int Float Boolean String Array Struct }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub identifier: String,
    pub position: Position,
}

impl ExpressionCodegen for Identifier {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let _symbol_table = compiler.symbol_table.clone();
        let (ptr, ty) = match _symbol_table.get_variable(&self.identifier) {
            Some(entry) => entry,
            None => {
                return Err(CompileError::identifier_not_found(
                    self.identifier.clone(),
                    self.position,
                ))
            }
        };

        Ok(Value::new(
            compiler.builder.build_load(
                ty.to_llvm_type(compiler.context),
                ptr,
                format!("load.{}", self.identifier).as_str(),
            ),
            ty.clone(),
        ))
    }
}

#[derive(Debug, Clone)]
pub struct IntLiteral {
    pub value: i64,
    pub position: Position,
}

impl ExpressionCodegen for IntLiteral {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        Ok(Value::new(
            compiler
                .context
                .i64_type()
                .const_int(self.value as u64, false)
                .into(),
            CodegenType::Int,
        ))
    }
}

#[derive(Debug, Clone)]
pub struct FloatLiteral {
    pub value: f64,
    pub position: Position,
}

impl ExpressionCodegen for FloatLiteral {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        Ok(Value::new(
            compiler.context.f64_type().const_float(self.value).into(),
            CodegenType::Float,
        ))
    }
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
    pub position: Position,
}

impl ExpressionCodegen for BooleanLiteral {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        Ok(Value::new(
            compiler
                .context
                .bool_type()
                .const_int(self.value as u64, false)
                .into(),
            CodegenType::Boolean,
        ))
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub position: Position,
}

impl ExpressionCodegen for StringLiteral {
    fn codegen<'a>(&self, _: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Element>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct Element {
    pub value: Expression,
    pub position: Position,
}

impl ExpressionCodegen for ArrayLiteral {
    fn codegen<'a>(&self, _: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct StructLiteral {
    pub name: Identifier,
    pub fields: BTreeMap<String, Field>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub value: Expression,
    pub position: Position,
}

impl ExpressionCodegen for StructLiteral {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let _symbol_table = compiler.symbol_table.clone();
        let struct_type = match _symbol_table.get_struct(&self.name.identifier) {
            Some(entry) => entry,
            None => {
                return Err(CompileError::struct_not_found(
                    self.name.identifier.clone(),
                    self.position,
                ))
            }
        };

        let mut values: Vec<BasicValueEnum> = Vec::new();

        for val in self.fields.clone() {
            values.push(val.1.value.codegen(compiler)?.llvm_value);
        }

        let ptr = compiler
            .builder
            .build_alloca(
                struct_type.0,
                format!("struct.{}", self.name.identifier).as_str(),
            )
            .as_basic_value_enum()
            .into_pointer_value();

        for (i, val) in values.iter().enumerate() {
            let field = unsafe {
                compiler.builder.build_in_bounds_gep(
                    compiler.context.i64_type(),
                    ptr,
                    &[compiler.context.i64_type().const_int(i as u64, false)],
                    format!("ptr.struct.{}.{i}", self.name.identifier).as_str(),
                )
            };
            compiler.builder.build_store(field, *val);
        }

        Ok(Value::new(
            ptr.as_basic_value_enum(),
            CodegenType::StructType(struct_type.1.clone()),
        ))
    }
}
