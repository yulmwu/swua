use crate::ast::{FunctionType, TyKind};
use inkwell::{types, values::PointerValue};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct SymbolTableEntry<'a> {
    /// Variable name, (llvm pointer, type)
    pub variables: HashMap<String, (PointerValue<'a>, TyKind)>,
    /// Function name, (llvm function type, function type)
    pub functions: HashMap<String, (types::FunctionType<'a>, FunctionType)>,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    pub entry: SymbolTableEntry<'a>,
    pub parent: Option<Box<SymbolTable<'a>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Self {
            entry: SymbolTableEntry::default(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: SymbolTable<'a>) -> Self {
        Self {
            entry: SymbolTableEntry::default(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn variables(&mut self) -> &mut HashMap<String, (PointerValue<'a>, TyKind)> {
        &mut self.entry.variables
    }

    pub fn functions(&mut self) -> &mut HashMap<String, (types::FunctionType<'a>, FunctionType)> {
        &mut self.entry.functions
    }

    pub fn insert_variable(&mut self, name: String, pointer: PointerValue<'a>, ty: TyKind) {
        self.entry.variables.insert(name, (pointer, ty));
    }

    pub fn insert_function(
        &mut self,
        name: String,
        llvm_function_type: types::FunctionType<'a>,
        function_type: FunctionType,
    ) {
        self.entry
            .functions
            .insert(name, (llvm_function_type, function_type));
    }

    pub fn get_variable(&self, name: &str) -> Option<&(PointerValue<'a>, TyKind)> {
        match self.entry.variables.get(name) {
            Some(value) => Some(value),
            None => match &self.parent {
                Some(parent) => parent.get_variable(name),
                None => None,
            },
        }
    }

    pub fn get_function(&self, name: &str) -> Option<&(types::FunctionType<'a>, FunctionType)> {
        match self.entry.functions.get(name) {
            Some(value) => Some(value),
            None => match &self.parent {
                Some(parent) => parent.get_function(name),
                None => None,
            },
        }
    }
}
