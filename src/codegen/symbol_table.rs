use crate::{
    codegen::{
        types::{CodegenType, FunctionType, StructType},
        CompileError, CompileResult,
    },
    Span,
};
use inkwell::{types, values::PointerValue};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Default)]
pub struct SymbolEntries<'a> {
    pub variables: BTreeMap<String, VariableEntry<'a>>,
    pub functions: BTreeMap<String, FunctionEntry<'a>>,
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
        name: String,
        ty: types::FunctionType<'a>,
        function_type: FunctionType,
    ) -> CompileResult<()> {
        if self.entries.functions.contains_key(&name) {
            return Err(CompileError::function_already_declared(
                name,
                function_type.span,
            ));
        }

        self.entries
            .functions
            .insert(name, FunctionEntry { ty, function_type });
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

    pub fn get_function(&self, name: &str) -> Option<FunctionEntry<'a>> {
        match self.entries.functions.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_function(name),
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
