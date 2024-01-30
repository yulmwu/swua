use crate::{
    codegen::{
        types::{CodegenType, FunctionType, StructType},
        CompileError, CompileResult,
    },
    utils::btreemap2::BTreeMap2,
    Span,
};
use inkwell::{types, values::PointerValue};
use std::collections::BTreeMap;

use super::types::FunctionParameterType;

#[derive(Debug, Clone, Default)]
pub struct SymbolEntries<'a> {
    pub variables: BTreeMap<String, VariableEntry<'a>>,
    pub functions: BTreeMap2<String, FunctionParameterType, FunctionEntry<'a>>,
    pub structs: BTreeMap<String, StructEntry<'a>>,
    pub type_aliases: BTreeMap<String, TypeAliasEntry>,
}

#[derive(Debug, Clone)]
pub struct VariableEntry<'a> {
    pub pointer: PointerValue<'a>,
    pub ty: CodegenType,
}

#[derive(Debug, Clone)]
pub struct FunctionEntry<'a> {
    pub name: String,
    pub ty: types::FunctionType<'a>,
    pub function_type: FunctionType,
}

#[derive(Debug, Clone)]
pub struct StructEntry<'a> {
    pub ty: types::StructType<'a>,
    pub struct_type: StructType,
}

#[derive(Debug, Clone)]
pub struct TypeAliasEntry {
    pub ty: CodegenType,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    pub entries: SymbolEntries<'a>,
    pub parent: Option<Box<SymbolTable<'a>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with_parent(parent: SymbolTable<'a>) -> Self {
        Self {
            entries: SymbolEntries::default(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn insert_variable(
        &mut self,
        name: String,
        ty: CodegenType,
        value: PointerValue<'a>,
        span: Span,
    ) -> CompileResult<()> {
        if self.entries.variables.contains_key(&name) {
            return Err(CompileError::variable_already_declared(name, span));
        }

        self.entries
            .variables
            .insert(name, VariableEntry { pointer: value, ty });
        Ok(())
    }

    pub fn insert_function(
        &mut self,
        alias: String,
        name: String,
        ty: types::FunctionType<'a>,
        function_type: FunctionType,
    ) -> CompileResult<()> {
        if self
            .entries
            .functions
            .insert(
                alias.clone(),
                function_type.parameters.clone(),
                FunctionEntry {
                    name,
                    ty,
                    function_type: function_type.clone(),
                },
            )
            .is_none()
        {
            return Err(CompileError::function_already_declared(
                alias,
                function_type.span,
            ));
        }
        Ok(())
    }

    pub fn insert_struct(
        &mut self,
        name: String,
        ty: types::StructType<'a>,
        struct_type: StructType,
    ) -> CompileResult<()> {
        if self.entries.structs.contains_key(&name) {
            return Err(CompileError::struct_already_declared(
                name,
                struct_type.span,
            ));
        }

        self.entries
            .structs
            .insert(name, StructEntry { ty, struct_type });
        Ok(())
    }

    pub fn insert_type_alias(
        &mut self,
        name: String,
        ty: CodegenType,
        span: Span,
    ) -> CompileResult<()> {
        if self.entries.type_aliases.contains_key(&name) {
            return Err(CompileError::type_already_declared(name, span));
        }

        self.entries
            .type_aliases
            .insert(name, TypeAliasEntry { ty });
        Ok(())
    }

    pub fn get_variable(&self, name: &str) -> Option<VariableEntry<'a>> {
        match self.entries.variables.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_variable(name),
                None => None,
            },
        }
    }

    pub fn get_function(
        &self,
        alias: &str,
        parameters: &FunctionParameterType,
    ) -> Option<FunctionEntry<'a>> {
        match self.entries.functions.get(alias, parameters) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_function(alias, parameters),
                None => None,
            },
        }
    }

    pub fn get_struct(&self, name: &str) -> Option<StructEntry<'a>> {
        match self.entries.structs.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_struct(name),
                None => None,
            },
        }
    }

    pub fn get_type_alias(&self, name: &str) -> Option<TypeAliasEntry> {
        match self.entries.type_aliases.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_type_alias(name),
                None => None,
            },
        }
    }
}
