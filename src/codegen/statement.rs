use super::{BlockExpression, CompileError, CompileResult, Expression, Identifier};
use crate::{
    display, AstType, CodegenType, Compiler, DisplayNode, ExpressionCodegen, FunctionType,
    Position, StatementCodegen, StructType, SymbolTable,
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
            Expression Let Function ExternalFunction Struct Return Type Declaration While
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
            Let Function ExternalFunction Struct Return Type Declaration While
        }

        writeln!(f)
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub ty: Option<AstType>,
    pub value: Expression,
    pub position: Position,
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
            format!("var.{}", self.name.identifier).as_str(),
        );

        compiler.builder.build_store(alloca, value.llvm_value);
        compiler.symbol_table.insert_variable(
            self.name.identifier.clone(),
            value.ty,
            alloca,
            self.name.position,
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
    pub body: BlockExpression,
    pub position: Position,
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
                position: self.position,
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
                parameter_name.position,
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
            return Err(CompileError::function_must_return_a_value(self.position));
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
    pub position: Position,
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
                position: self.position,
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
    pub position: Position,
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
            position: self.position,
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
    pub position: Position,
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
pub struct TypeDeclaration {
    pub name: Identifier,
    pub ty: AstType,
    pub position: Position,
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
    pub position: Position,
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
    pub body: BlockExpression,
    pub position: Position,
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
