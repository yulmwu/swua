use super::{
    symbol_table::SymbolTable, types::AstType, CompileError, CompileResult, Expression, Identifier,
};
use crate::{
    display, utils::hash, CodegenType, Compiler, CurrentFunction, DisplayNode, ExpressionCodegen,
    FunctionType, Span, StatementCodegen, StructType,
};
use inkwell::{types::BasicType, IntPredicate};
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
    While(While),
    For(For),
    Ellipsis,
}

impl StatementCodegen for Statement {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        macro_rules! inner {
            ($($ident:ident)*) => {
                match self {
                    $(
                        Statement::$ident(statement) => { statement.codegen(compiler)?; },
                    )*
                    Statement::Ellipsis => {},
                }
            };
        }

        inner! {
            Expression Let Function ExternalFunction Struct Return If Type While For
        }

        Ok(())
    }
}

impl DisplayNode for Statement {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        macro_rules! inner {
            ($($ident:ident)*) => {
                match self {
                    $(
                        Statement::$ident(statement) => { statement.display(f, indent)?; },
                    )*
                    Statement::Expression(expression) => {
                        display::indent(f, indent)?;
                        expression.display(f, indent)?;
                    },
                    Statement::Ellipsis => {
                        display::indent(f, indent)?;
                        writeln!(f, "...")?;
                    }
                }
            };
        }

        inner! {
            Let Function ExternalFunction Struct Return If Type While For
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
        self.value.display(f, indent)
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

        let mut parameters_codegen_type = Vec::new();

        for parameter in self.parameters.clone() {
            parameters_codegen_type
                .push(parameter.ty.kind.to_codegen_type(&compiler.symbol_table)?);
        }

        let codegen_function_type = FunctionType {
            name: self.name.identifier.clone(),
            parameters: parameters_codegen_type.clone().into(),
            return_type: Box::new(return_type.clone()),
            span: self.span,
        };

        let llvm_function_name = if self.name.identifier == "main" {
            // TODO
            "main".to_string()
        } else {
            format!(
                "{}_{:x}",
                self.name.identifier,
                hash(&codegen_function_type)
            )
        };

        let function =
            compiler
                .module
                .add_function(llvm_function_name.as_str(), function_type, None);

        compiler.current_function = Some(CurrentFunction {
            function,
            return_type: return_type.clone(),
        });

        let basic_block = compiler.context.append_basic_block(function, "entry");

        compiler.builder.position_at_end(basic_block);

        compiler.symbol_table.insert_function(
            self.name.identifier.clone(),
            self.name.identifier.clone(),
            function_type,
            codegen_function_type,
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

        self.body.codegen(compiler)?;

        if return_type != CodegenType::Void && compiler.current_return.is_none() {
            return Err(CompileError::expected("return", self.span));
        } else {
            compiler.current_return = None;
        }

        if return_type == CodegenType::Void {
            compiler.builder.build_return(None);
        }

        compiler.symbol_table = original_symbol_table;

        Ok(())
    }
}

impl DisplayNode for FunctionDefinition {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "define ")?;
        self.name.display(f, indent)?;

        if !self.parameters.is_empty() {
            write!(f, "(")?;
            for (i, parameter) in self.parameters.iter().enumerate() {
                parameter.name.display(f, indent)?;
                write!(f, " {}", parameter.ty.kind)?;
                if i != self.parameters.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")?;
        }

        write!(f, " -> {} =", self.return_type.kind)?;

        self.body.display(f, indent)
    }
}

#[derive(Debug, Clone)]
pub struct ExternalFunctionDeclaration {
    pub alias: Option<Identifier>,
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

        let mut parameters_codegen_type = Vec::new();

        for parameter in self.parameters.clone() {
            parameters_codegen_type.push(parameter.kind.to_codegen_type(&compiler.symbol_table)?);
        }

        let codegen_function_type = FunctionType {
            name: self.name.identifier.clone(),
            parameters: parameters_codegen_type.into(),
            return_type: Box::new(return_type),
            span: self.span,
        };

        compiler.module.add_function(
            &format!(
                "{}_{:x}",
                self.name.identifier,
                hash(&codegen_function_type)
            ),
            function_type,
            None,
        );

        compiler.symbol_table.insert_function(
            self.alias
                .clone()
                .unwrap_or_else(|| self.name.clone())
                .identifier,
            self.name.identifier.clone(),
            function_type,
            codegen_function_type,
        )?;

        Ok(())
    }
}

impl DisplayNode for ExternalFunctionDeclaration {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "extern ")?;
        self.name.display(f, indent)?;
        write!(f, "(")?;
        for (i, parameter) in self.parameters.iter().enumerate() {
            write!(f, "{}", parameter.kind)?;
            if i != self.parameters.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ") -> {}", self.return_type.kind)
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
        for (name, ty) in self.fields.iter() {
            writeln!(f)?;
            display::indent(f, indent + 1)?;
            write!(f, "| {} {}", name, ty.kind)?;
        }
        writeln!(f)?;
        display::indent(f, indent)
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

        let CurrentFunction { return_type, .. } = compiler.current_function.clone().unwrap();

        if value.ty != return_type {
            return Err(CompileError::type_mismatch(
                return_type,
                value.ty,
                self.value.clone().into(),
            ));
        }

        compiler.builder.build_return(Some(&value.llvm_value));
        compiler.current_return = Some(value);

        Ok(())
    }
}

impl DisplayNode for ReturnStatement {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "return ")?;
        self.value.display(f, indent)
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

        let function = compiler.current_function.clone().unwrap().function;

        let then_block = compiler.context.append_basic_block(function, "then");
        let else_block = compiler.context.append_basic_block(function, "else");
        let merge_block = compiler.context.append_basic_block(function, "merge");

        compiler.builder.build_conditional_branch(
            condition.llvm_value.into_int_value(),
            then_block,
            else_block,
        );

        compiler.builder.position_at_end(then_block);
        self.consequence.codegen(compiler)?;

        let mut if_terminated = false;

        if compiler
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            compiler.builder.build_unconditional_branch(merge_block);
        } else {
            if_terminated = true;
        }

        compiler.builder.position_at_end(else_block);
        if let Some(alternative) = self.alternative.clone() {
            alternative.codegen(compiler)?;

            if compiler
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_none()
                && if_terminated
            {
                return Err(CompileError::expected("terminator", alternative.span));
            }
        } else if if_terminated {
            return Err(CompileError::else_clause_is_required(self.span));
        }

        if !if_terminated {
            compiler.builder.build_unconditional_branch(merge_block);
        }

        compiler.builder.position_at_end(merge_block);

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
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let ty = self.ty.kind.to_codegen_type(&compiler.symbol_table)?;

        compiler.symbol_table.insert_type_alias(
            self.name.identifier.clone(),
            ty.clone(),
            self.span,
        )?;

        Ok(())
    }
}

impl DisplayNode for TypeDeclaration {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "type ")?;
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
        let function = compiler.current_function.clone().unwrap().function;

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
        self.condition.display(f, indent)?;
        writeln!(f)?;
        self.body.display(f, indent)
    }
}

#[derive(Debug, Clone)]
pub struct For {
    pub initialization: ForInitialization,
    pub condition: Expression,
    pub increment: Expression,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForInitialization {
    pub name: Identifier,
    pub value: Expression,
    pub span: Span,
}

impl StatementCodegen for For {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let function = compiler.current_function.clone().unwrap().function;

        let condition_block = compiler.context.append_basic_block(function, "for.cond");
        let body_block = compiler.context.append_basic_block(function, "for.body");
        let increment_block = compiler.context.append_basic_block(function, "for.inc");
        let end_block = compiler.context.append_basic_block(function, "for.end");

        let value = self.initialization.value.codegen(compiler)?;
        let alloca = compiler.builder.build_alloca(
            value.ty.to_llvm_type(compiler.context),
            &self.initialization.name.identifier,
        );
        compiler.builder.build_store(alloca, value.llvm_value);

        let original_symbol_table = compiler.symbol_table.clone();
        compiler.symbol_table = SymbolTable::new_with_parent(compiler.symbol_table.clone());

        compiler.symbol_table.insert_variable(
            self.initialization.name.identifier.clone(),
            value.ty,
            alloca,
            self.initialization.name.span,
        )?;

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
            "for.cond",
        );
        compiler
            .builder
            .build_conditional_branch(condition, body_block, end_block);

        compiler.builder.position_at_end(body_block);

        for statement in self.body.statements.clone() {
            statement.codegen(compiler)?;
        }

        compiler.builder.build_unconditional_branch(increment_block);

        compiler.builder.position_at_end(increment_block);
        self.increment.codegen(compiler)?;

        compiler.builder.build_unconditional_branch(condition_block);

        compiler.builder.position_at_end(end_block);

        compiler.symbol_table = original_symbol_table;

        Ok(())
    }
}

impl DisplayNode for For {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        display::indent(f, indent)?;
        write!(f, "for ")?;
        self.initialization.name.display(f, indent)?;
        write!(f, " = ")?;
        self.initialization.value.display(f, indent)?;
        write!(f, "; ")?;
        self.condition.display(f, indent)?;
        write!(f, "; ")?;
        self.increment.display(f, indent)?;
        self.body.display(f, indent)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

impl StatementCodegen for Block {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let original_symbol_table = compiler.symbol_table.clone();
        compiler.symbol_table = SymbolTable::new_with_parent(compiler.symbol_table.clone());

        for statement in self.statements.clone() {
            statement.codegen(compiler)?;
        }

        compiler.symbol_table = original_symbol_table;

        Ok(())
    }
}

impl DisplayNode for Block {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        writeln!(f)?;
        for statement in self.statements.clone() {
            statement.display(f, indent + 1)?;
        }
        display::indent(f, indent)
    }
}
