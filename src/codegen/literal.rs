use super::{types::ArrayType, CompileError, CompileResult, Expression};
use crate::{
    display, CodegenType, Compiler, DisplayNode, ExpressionCodegen, Span, StructType, Value,
};
use inkwell::{
    types::BasicType,
    values::{BasicValue, BasicValueEnum},
};
use std::{collections::BTreeMap, fmt};

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

impl From<Literal> for Span {
    fn from(literal: Literal) -> Self {
        macro_rules! inner {
            ($($ident:ident)*) => {
                match literal {
                    $(
                        Literal::$ident(literal) => literal.span,
                    )*
                }
            };
        }

        inner! { Identifier Int Float Boolean String Array Struct }
    }
}

impl DisplayNode for Literal {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        macro_rules! inner {
            ($($ident:ident)*) => {
                match self {
                    $(
                        Literal::$ident(literal) => literal.display(f, indent),
                    )*
                }
            };
        }

        inner! { Identifier Int Float Boolean String Array Struct }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub identifier: String,
    pub span: Span,
}

impl ExpressionCodegen for Identifier {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let _symbol_table = compiler.symbol_table.clone();
        let entry = match _symbol_table.get_variable(&self.identifier) {
            Some(entry) => entry,
            None => {
                return Err(CompileError::identifier_not_found(
                    self.identifier.clone(),
                    self.span,
                ))
            }
        };

        Ok(Value::new(
            compiler.builder.build_load(
                entry.ty.to_llvm_type(compiler.context),
                entry.pointer,
                format!("load.{}", self.identifier).as_str(),
            ),
            entry.ty.clone(),
        ))
    }
}

impl DisplayNode for Identifier {
    fn display(&self, f: &mut fmt::Formatter<'_>, _: usize) -> fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

#[derive(Debug, Clone)]
pub struct IntLiteral {
    pub value: i64,
    pub span: Span,
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

impl DisplayNode for IntLiteral {
    fn display(&self, f: &mut fmt::Formatter<'_>, _: usize) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct FloatLiteral {
    pub value: f64,
    pub span: Span,
}

impl ExpressionCodegen for FloatLiteral {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        Ok(Value::new(
            compiler.context.f64_type().const_float(self.value).into(),
            CodegenType::Float,
        ))
    }
}

impl DisplayNode for FloatLiteral {
    fn display(&self, f: &mut fmt::Formatter<'_>, _: usize) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    pub value: bool,
    pub span: Span,
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

impl DisplayNode for BooleanLiteral {
    fn display(&self, f: &mut fmt::Formatter<'_>, _: usize) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub span: Span,
}

impl ExpressionCodegen for StringLiteral {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let string = compiler
            .builder
            .build_global_string_ptr(self.value.as_str(), ".str");

        Ok(Value::new(
            string.as_basic_value_enum(),
            CodegenType::String,
        ))
    }
}

impl DisplayNode for StringLiteral {
    fn display(&self, f: &mut fmt::Formatter<'_>, _: usize) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
    pub span: Span,
}

impl ExpressionCodegen for ArrayLiteral {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let mut values = Vec::new();
        let mut element_type = None;

        for val in self.elements.clone() {
            let value = val.codegen(compiler)?;
            values.push(value.llvm_value);

            match element_type.clone() {
                Some(ty) => {
                    if ty != value.ty {
                        return Err(CompileError::type_mismatch(ty, value.ty, val.into()));
                    }
                }
                None => element_type = Some(value.ty),
            }
        }

        let array_type = match element_type {
            Some(ty) => ty,
            None => {
                return Err(CompileError::array_must_have_at_least_one_element(
                    self.span,
                ))
            }
        };

        let array = array_type
            .to_llvm_type(compiler.context)
            .array_type(values.len() as u32);
        let ptr = compiler.builder.build_array_alloca(
            array,
            compiler
                .context
                .i64_type()
                .const_int(values.len() as u64, true),
            ".array",
        );

        for (i, val) in values.iter().enumerate() {
            let ptr = unsafe {
                compiler.builder.build_in_bounds_gep(
                    array_type.to_llvm_type(compiler.context),
                    ptr,
                    &[compiler.context.i64_type().const_int(i as u64, true)],
                    format!("ptr.array.{}", i).as_str(),
                )
            };
            compiler.builder.build_store(ptr, *val);
        }

        Ok(Value::new(
            ptr.as_basic_value_enum(),
            CodegenType::Array(ArrayType {
                ty: Box::new(array_type),
                len: Some(values.len()),
                span: self.span,
            }),
        ))
    }
}

impl DisplayNode for ArrayLiteral {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write!(f, "[")?;
        for (i, val) in self.elements.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            val.display(f, indent)?;
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone)]
pub struct StructLiteral {
    pub name: Identifier,
    pub fields: BTreeMap<String, Expression>,
    pub span: Span,
}

impl ExpressionCodegen for StructLiteral {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let _symbol_table = compiler.symbol_table.clone();
        let entry = match _symbol_table.get_struct(&self.name.identifier)
        {
            Some(entry) => entry,
            None => {
                return Err(CompileError::struct_not_found(
                    self.name.identifier.clone(),
                    self.span,
                ))
            }
        };

        if self.fields.len() != entry.struct_type.fields.len() {
            return Err(CompileError::wrong_number_of_fields(
                entry.struct_type.fields.len(),
                self.fields.len(),
                self.span,
            ));
        }

        let mut values: Vec<BasicValueEnum> = Vec::new();
        let mut feilds_type = BTreeMap::new();

        for (i, val) in self.fields.iter().enumerate() {
            let value = val.1.codegen(compiler)?;
            values.push(value.llvm_value);

            let field_type = match entry.struct_type.fields.get(val.0) {
                Some((_, ty)) => ty.clone(),
                None => return Err(CompileError::field_not_found(val.0.clone(), self.span)),
            };

            if field_type != value.ty {
                return Err(CompileError::type_mismatch(
                    field_type,
                    value.ty,
                    val.1.clone().into(),
                ));
            }

            feilds_type.insert(val.0.clone(), (i, value.ty));
        }

        let ptr = compiler.builder.build_alloca(
            entry.ty,
            format!("struct.{}", self.name.identifier).as_str(),
        );

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
            CodegenType::Struct(StructType {
                name: self.name.identifier.clone(),
                fields: feilds_type,
                span: self.span,
            }),
        ))
    }
}

impl DisplayNode for StructLiteral {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write!(f, "{} {{", self.name.identifier)?;
        for (i, val) in self.fields.iter().enumerate() {
            writeln!(f)?;
            display::indent(f, indent + 1)?;
            write!(f, "{}: ", val.0)?;
            val.1.display(f, indent + 1)?;
            if i != self.fields.len() - 1 {
                write!(f, ",")?;
            }
        }
        writeln!(f)?;
        display::indent(f, indent)?;
        write!(f, "}}")
    }
}
