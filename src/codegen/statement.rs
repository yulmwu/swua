use super::{
    symbol_table::SymbolTable, types::AstType, CompileError, CompileResult, Expression, Identifier,
};
use crate::{
    display, CodegenType, Compiler, DisplayNode, ExpressionCodegen, FunctionType, Span,
    StatementCodegen, StructType, Value,
};
use inkwell::{types::BasicType, values::BasicValue, IntPredicate};
use std::{collections::BTreeMap, fmt};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Function(FunctionDefinition),
    ExternalFunction(ExternalFunctionDeclaration),
    Struct(StructDeclaration),
    Return(ReturnStatement),
    If(IfStatement),
    Type(TypeDeclaration),
    Declaration(Declaration),
    While(While),
}

impl StatementCodegen for Statement {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        macro_rules! inner {
            ($($ident:ident)*) => {
                match self {
                    $(
                        Statement::$ident(statement) => { statement.codegen(compiler)?; },
                    )*
                }
            };
        }

        inner! {
            Expression Let Function ExternalFunction Struct Return If Type Declaration While
        }

        Ok(())
    }
}

impl DisplayNode for Statement {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        macro_rules! inner {
            ($($ident:ident)*) => {
                match self {
                    Statement::Expression(expression) => {
                        display::indent(f, indent)?;
                        expression.display(f, indent)?;
                        write!(f, ";")?;
                    },
                    $(
                        Statement::$ident(statement) => { statement.display(f, indent)?; },
                    )*
                }
            };
        }

        inner! {
            Let Function ExternalFunction Struct Return If Type Declaration While
        }

        writeln!(f)
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub ty: Option<AstType>,
    pub value: Expression,
    pub span: Span,
}

impl StatementCodegen for LetStatement {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let value = self.value.codegen(compiler)?;
        if let Some(ty) = self.ty.clone() {
            let inferred_ty = ty.kind.to_codegen_type(&compiler.symbol_table)?;
            if inferred_ty != value.ty {
                return Err(CompileError::type_mismatch(
                    inferred_ty,
                    value.ty,
                    self.value.clone().into(),
                ));
            }
        }

        let alloca = compiler.builder.build_alloca(
            value.ty.to_llvm_type(compiler.context),
            &self.name.identifier,
        );

        compiler.builder.build_store(alloca, value.llvm_value);

        compiler.symbol_table.insert_variable(
            self.name.identifier.clone(),
            value.ty,
            alloca,
            self.name.span,
        )?;

        Ok(())
    }
}

impl DisplayNode for LetStatement {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "let ")?;
        self.name.display(f, indent)?;
        if let Some(ty) = self.ty.clone() {
            write!(f, ": {}", ty.kind)?;
        }
        write!(f, " = ")?;
        self.value.display(f, indent)?;
        write!(f, ";")
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: AstType,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: AstType,
}

impl StatementCodegen for FunctionDefinition {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let mut parameters = Vec::new();

        for parameter in self.parameters.clone() {
            parameters.push(
                parameter
                    .ty
                    .kind
                    .to_codegen_type(&compiler.symbol_table)?
                    .to_llvm_type(compiler.context)
                    .into(),
            );
        }

        let return_type = self
            .return_type
            .kind
            .to_codegen_type(&compiler.symbol_table)?;
        let function_type = return_type
            .to_llvm_type(compiler.context)
            .fn_type(parameters.as_slice(), false);

        let function =
            compiler
                .module
                .add_function(self.name.identifier.as_str(), function_type, None);

        let basic_block = compiler.context.append_basic_block(function, "entry");

        compiler.builder.position_at_end(basic_block);

        let mut parameters_codegen_type = Vec::new();

        for parameter in self.parameters.clone() {
            parameters_codegen_type
                .push(parameter.ty.kind.to_codegen_type(&compiler.symbol_table)?);
        }

        compiler.symbol_table.insert_function(
            self.name.identifier.clone(),
            function_type,
            FunctionType {
                name: self.name.identifier.clone(),
                parameters: parameters_codegen_type.clone(),
                return_type: Box::new(return_type.clone()),
                span: self.span,
            },
        )?;

        let original_symbol_table = compiler.symbol_table.clone();
        compiler.symbol_table = SymbolTable::new_with_parent(compiler.symbol_table.clone());

        for (i, parameter) in function.get_param_iter().enumerate() {
            let parameter_name = self.parameters[i].name.clone();
            let alloca = compiler.builder.build_alloca(
                compiler.context.i64_type(),
                format!("arg.{}", parameter_name.identifier).as_str(),
            );
            compiler.builder.build_store(alloca, parameter);

            compiler.symbol_table.insert_variable(
                parameter_name.identifier.clone(),
                parameters_codegen_type[i].clone(),
                alloca,
                parameter_name.span,
            )?;
        }

        for statement in self.body.statements.clone() {
            if let Statement::Return(return_statement) = statement {
                let value = return_statement.value.codegen(compiler)?;

                if value.ty != return_type {
                    return Err(CompileError::type_mismatch(
                        return_type,
                        value.ty,
                        return_statement.value.clone().into(),
                    ));
                }

                compiler.builder.build_return(Some(&value.llvm_value));
                compiler.symbol_table = original_symbol_table;
                return Ok(());
            }

            statement.codegen(compiler)?;
        }

        if return_type != CodegenType::Void {
            return Err(CompileError::function_must_return_a_value(self.span));
        }

        compiler.symbol_table = original_symbol_table;

        Ok(())
    }
}

impl DisplayNode for FunctionDefinition {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "fn ")?;
        self.name.display(f, indent)?;
        write!(f, "(")?;
        for (i, parameter) in self.parameters.iter().enumerate() {
            parameter.name.display(f, indent)?;
            write!(f, ": {}", parameter.ty.kind)?;
            if i != self.parameters.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ") -> {} ", self.return_type.kind)?;
        self.body.display(f, indent)
    }
}

#[derive(Debug, Clone)]
pub struct ExternalFunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<AstType>,
    pub return_type: AstType,
    pub span: Span,
}

impl StatementCodegen for ExternalFunctionDeclaration {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let mut parameters = Vec::new();

        for parameter in self.parameters.clone() {
            parameters.push(
                parameter
                    .kind
                    .to_codegen_type(&compiler.symbol_table)?
                    .to_llvm_type(compiler.context)
                    .into(),
            );
        }

        let return_type = self
            .return_type
            .kind
            .to_codegen_type(&compiler.symbol_table)?;

        let function_type = return_type
            .to_llvm_type(compiler.context)
            .fn_type(parameters.as_slice(), false);

        compiler
            .module
            .add_function(self.name.identifier.as_str(), function_type, None);

        let mut parameters_codegen_type = Vec::new();

        for parameter in self.parameters.clone() {
            parameters_codegen_type.push(parameter.kind.to_codegen_type(&compiler.symbol_table)?);
        }

        compiler.symbol_table.insert_function(
            self.name.identifier.clone(),
            function_type,
            FunctionType {
                name: self.name.identifier.clone(),
                parameters: parameters_codegen_type.clone(),
                return_type: Box::new(return_type),
                span: self.span,
            },
        )?;

        Ok(())
    }
}

impl DisplayNode for ExternalFunctionDeclaration {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "extern fn ")?;
        self.name.display(f, indent)?;
        write!(f, "(")?;
        for (i, parameter) in self.parameters.iter().enumerate() {
            write!(f, "{}", parameter.kind)?;
            if i != self.parameters.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ") -> {};", self.return_type.kind)
    }
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: Identifier,
    pub fields: BTreeMap<String, AstType>,
    pub span: Span,
}

impl StatementCodegen for StructDeclaration {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let mut fields = BTreeMap::new();

        for (i, (name, ty)) in self.fields.iter().enumerate() {
            fields.insert(
                name.clone(),
                (i, ty.kind.to_codegen_type(&compiler.symbol_table)?.clone()),
            );
        }

        let struct_type = StructType {
            name: self.name.identifier.clone(),
            fields,
            span: self.span,
        };
        let struct_llvm_type = compiler
            .context
            .opaque_struct_type(self.name.identifier.as_str());
        struct_llvm_type.set_body(
            struct_type
                .fields
                .iter()
                .map(|(_, (_, ty))| ty.to_llvm_type(compiler.context))
                .collect::<Vec<_>>()
                .as_slice(),
            false,
        );

        compiler.symbol_table.insert_struct(
            self.name.identifier.clone(),
            struct_llvm_type,
            struct_type,
        )?;

        Ok(())
    }
}

impl DisplayNode for StructDeclaration {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "struct ")?;
        self.name.display(f, indent)?;
        write!(f, " {{ ")?;
        for (i, (name, ty)) in self.fields.iter().enumerate() {
            writeln!(f)?;
            display::indent(f, indent + 1)?;
            write!(f, "{}: {}", name, ty.kind)?;
            if i != self.fields.len() - 1 {
                write!(f, ", ")?;
            }
        }
        writeln!(f)?;
        display::indent(f, indent)?;
        write!(f, "}};")
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Expression,
    pub span: Span,
}

impl StatementCodegen for ReturnStatement {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let value = self.value.codegen(compiler)?;
        compiler.builder.build_return(Some(&value.llvm_value));

        Ok(())
    }
}

impl DisplayNode for ReturnStatement {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "return ")?;
        self.value.display(f, indent)?;
        write!(f, ";")
    }
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Box<Expression>,
    pub consequence: Block,
    pub alternative: Option<Block>,
    pub span: Span,
}

impl StatementCodegen for IfStatement {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let condition = self.condition.codegen(compiler)?;
        if condition.ty != CodegenType::Boolean {
            return Err(CompileError::expected("boolean", self.span));
        }

        let function = compiler
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let then_block = compiler.context.append_basic_block(function, "then");
        let else_block = compiler.context.append_basic_block(function, "else");
        let merge_block = compiler.context.append_basic_block(function, "merge");

        compiler.builder.build_conditional_branch(
            condition.llvm_value.into_int_value(),
            then_block,
            else_block,
        );

        compiler.builder.position_at_end(then_block);

        let then = self.consequence.codegen(compiler)?;
        compiler.builder.build_unconditional_branch(merge_block);

        let then_block = compiler.builder.get_insert_block().unwrap();

        compiler.builder.position_at_end(else_block);

        let else_ = match self.alternative.clone() {
            Some(expr) => {
                let else_ = expr.codegen(compiler)?;

                if then.ty != else_.ty {
                    return Err(CompileError::type_mismatch(then.ty, else_.ty, self.span));
                }

                else_
            }
            None => Value::new(
                compiler
                    .context
                    .i64_type()
                    .const_int(0, false)
                    .as_basic_value_enum(),
                CodegenType::Void,
            ),
        };
        compiler.builder.build_unconditional_branch(merge_block);

        let else_block = compiler.builder.get_insert_block().unwrap();

        compiler.builder.position_at_end(merge_block);

        let phi = compiler
            .builder
            .build_phi(then.llvm_value.get_type(), "iftmp");
        phi.add_incoming(&[
            (&then.llvm_value, then_block),
            (&else_.llvm_value, else_block),
        ]);

        Ok(())
    }
}

impl DisplayNode for IfStatement {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "if ")?;
        self.condition.display(f, indent)?;
        write!(f, " ")?;
        self.consequence.display(f, indent)?;
        if let Some(alternative) = self.alternative.clone() {
            write!(f, " else ")?;
            alternative.display(f, indent)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub name: Identifier,
    pub ty: AstType,
    pub span: Span,
}

impl StatementCodegen for TypeDeclaration {
    fn codegen(&self, _: &mut Compiler) -> CompileResult<()> {
        todo!()
    }
}

impl DisplayNode for TypeDeclaration {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        self.name.display(f, indent)?;
        write!(f, " = {}", self.ty.kind)
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: Identifier,
    pub ty: AstType,
    pub span: Span,
}

impl StatementCodegen for Declaration {
    fn codegen(&self, _: &mut Compiler) -> CompileResult<()> {
        todo!()
    }
}

impl DisplayNode for Declaration {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        self.name.display(f, indent)?;
        write!(f, " = {}", self.ty.kind)
    }
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expression,
    pub body: Block,
    pub span: Span,
}

impl StatementCodegen for While {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let function = compiler
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let condition_block = compiler.context.append_basic_block(function, "while.cond");
        let body_block = compiler.context.append_basic_block(function, "while.body");
        let end_block = compiler.context.append_basic_block(function, "while.end");

        compiler.builder.build_unconditional_branch(condition_block);

        compiler.builder.position_at_end(condition_block);
        let condition = self.condition.codegen(compiler)?;
        if condition.ty != CodegenType::Boolean {
            return Err(CompileError::type_mismatch(
                CodegenType::Boolean,
                condition.ty,
                self.condition.clone().into(),
            ));
        }

        let condition = compiler.builder.build_int_compare(
            IntPredicate::NE,
            condition.llvm_value.into_int_value(),
            compiler.context.i64_type().const_int(0, false),
            "while.cond",
        );
        compiler
            .builder
            .build_conditional_branch(condition, body_block, end_block);

        compiler.builder.position_at_end(body_block);
        self.body.codegen(compiler)?;

        compiler.builder.build_unconditional_branch(condition_block);

        compiler.builder.position_at_end(end_block);

        Ok(())
    }
}

impl DisplayNode for While {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        writeln!(f, "while ")?;
        self.condition.display(f, indent + 1)?;
        writeln!(f, " {{")?;
        self.body.display(f, indent + 1)?;
        display::indent(f, indent)?;
        write!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

// not a expression
impl ExpressionCodegen for Block {
    fn codegen<'a>(&self, compiler: &mut Compiler<'a>) -> CompileResult<Value<'a>> {
        let original_symbol_table = compiler.symbol_table.clone();
        compiler.symbol_table = SymbolTable::new_with_parent(compiler.symbol_table.clone());

        for statement in self.statements.clone() {
            if let Statement::Return(return_statement) = statement {
                let value = return_statement.value.codegen(compiler)?;
                compiler.symbol_table = original_symbol_table;
                return Ok(value);
            }

            statement.codegen(compiler)?;
        }

        compiler.symbol_table = original_symbol_table;

        Ok(Value::new(
            compiler.context.i64_type().const_int(0, false).into(),
            CodegenType::Void,
        ))
    }
}

impl DisplayNode for Block {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        writeln!(f, "{{")?;
        for statement in self.statements.clone() {
            statement.display(f, indent + 1)?;
        }
        display::indent(f, indent)?;
        write!(f, "}}")
    }
}
