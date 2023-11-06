use super::{BlockExpression, CompileError, CompileResult, Expression, Identifier};
use crate::{
    AstType, Compiler, ExpressionCodegen, FunctionType, Position, StatementCodegen, StructType,
    SymbolTable,
};
use inkwell::types::BasicType;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    LetStatement(LetStatement),
    FunctionDefinition(FunctionDefinition),
    ExternalFunctionDeclaration(ExternalFunctionDeclaration),
    StructDeclaration(StructDeclaration),
    Return(ReturnStatement),
    TypeDeclaration(TypeDeclaration),
    Declaration(Declaration),
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
            Expression LetStatement FunctionDefinition ExternalFunctionDeclaration StructDeclaration Return TypeDeclaration Declaration
        }

        Ok(())
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
                    self.position, // TODO: Use .position by implementing the GetPosition trait for each AST
                ));
            }
        }

        let alloca = compiler.builder.build_alloca(
            value.ty.to_llvm_type(compiler.context),
            format!("var.{}", self.name.identifier).as_str(),
        );

        compiler.builder.build_store(alloca, value.llvm_value);
        compiler
            .symbol_table
            .insert_variable(self.name.identifier.clone(), value.ty, alloca);

        Ok(())
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
    pub position: Position,
}

impl StatementCodegen for FunctionDefinition {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let mut parameters = Vec::new();

        for parameter in self.parameters.clone() {
            let ty = parameter
                .ty
                .kind
                .to_codegen_type(&compiler.symbol_table)?
                .to_llvm_type(compiler.context);
            parameters.push(ty.into());
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

        // let parameters_codegen_type = self
        //     .parameters
        //     .iter()
        //     .map(|parameter| parameter.ty.kind.to_codegen_type(&compiler.symbol_table)?)
        //     .collect::<Vec<_>>();
        let mut parameters_codegen_type = Vec::new();

        for parameter in self.parameters.clone() {
            let ty = parameter.ty.kind.to_codegen_type(&compiler.symbol_table)?;
            parameters_codegen_type.push(ty);
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
        );

        let original_symbol_table = compiler.symbol_table.clone();
        compiler.symbol_table = SymbolTable::new_with_parent(compiler.symbol_table.clone());

        for (i, parameter) in function.get_param_iter().enumerate() {
            let alloca = compiler.builder.build_alloca(
                compiler.context.i64_type(),
                format!("arg.{}", self.parameters[i].name.identifier).as_str(),
            );
            compiler.builder.build_store(alloca, parameter);

            compiler.symbol_table.insert_variable(
                self.parameters[i].name.identifier.clone(),
                parameters_codegen_type[i].clone(),
                alloca,
            );
        }

        for statement in self.body.statements.clone() {
            if let Statement::Return(return_statement) = statement {
                let value = return_statement.value.codegen(compiler)?;
                compiler.builder.build_return(Some(&value.llvm_value));
                compiler.symbol_table = original_symbol_table;
                return Ok(());
            }

            statement.codegen(compiler)?;
        }

        compiler.symbol_table = original_symbol_table;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExternalFunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<ExternalFunctionParameter>,
    pub return_type: AstType,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct ExternalFunctionParameter {
    pub ty: AstType,
    pub position: Position,
}

impl StatementCodegen for ExternalFunctionDeclaration {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let mut parameters = Vec::new();

        for parameter in self.parameters.clone() {
            let ty = parameter.ty.kind.to_codegen_type(&compiler.symbol_table)?;
            parameters.push(ty.to_llvm_type(compiler.context).into());
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
            let ty = parameter.ty.kind.to_codegen_type(&compiler.symbol_table)?;
            parameters_codegen_type.push(ty);
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
        );

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: Identifier,
    pub fields: BTreeMap<String, FieldTy>,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct FieldTy {
    pub ty: AstType,
    pub position: Position,
}

impl StatementCodegen for StructDeclaration {
    fn codegen(&self, compiler: &mut Compiler) -> CompileResult<()> {
        let mut fields = BTreeMap::new();

        for (i, (name, ty)) in self.fields.iter().enumerate() {
            fields.insert(
                name.clone(),
                (
                    i,
                    ty.ty.kind.to_codegen_type(&compiler.symbol_table)?.clone(),
                ),
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
        );

        Ok(())
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
