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
    pub symbols: BTreeMap<String, (PointerValue<'a>, CodegenType)>,
    pub functions: BTreeMap<String, (types::FunctionType<'a>, FunctionType)>,
    pub structs: BTreeMap<String, (types::StructType<'a>, StructType)>,
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
        if self.entries.symbols.contains_key(&name) {
            return Err(CompileError::variable_already_declared(name, span));
        }

        self.entries.symbols.insert(name, (value, ty));
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

        self.entries.functions.insert(name, (ty, function_type));
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

        self.entries.structs.insert(name, (ty, struct_type));
        Ok(())
    }

    pub fn get_variable(&self, name: &str) -> Option<(PointerValue<'a>, CodegenType)> {
        match self.entries.symbols.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_variable(name),
                None => None,
            },
        }
    }

    pub fn get_function(&self, name: &str) -> Option<(types::FunctionType<'a>, FunctionType)> {
        match self.entries.functions.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_function(name),
                None => None,
            },
        }
    }

    pub fn get_struct(&self, name: &str) -> Option<(types::StructType<'a>, StructType)> {
        match self.entries.structs.get(name) {
            Some(entry) => Some(entry.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_struct(name),
                None => None,
            },
        }
    }
}
