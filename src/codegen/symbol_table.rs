use crate::ast::{FunctionType, TyKind};
use inkwell::{types, values::PointerValue};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct SymbolTableEntry<'a> {
    pub variables: HashMap<String, VariableEntry<'a>>,
    pub functions: HashMap<String, FunctionEntry<'a>>,
}

#[derive(Debug, Clone)]
pub struct VariableEntry<'a> {
    pub pointer: PointerValue<'a>,
    pub ty: TyKind,
}

impl<'a> VariableEntry<'a> {
    pub fn new(pointer: PointerValue<'a>, ty: TyKind) -> Self {
        Self { pointer, ty }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionEntry<'a> {
    pub function_type: types::FunctionType<'a>,
    pub ty: FunctionType,
}

impl<'a> FunctionEntry<'a> {
    pub fn new(function_type: types::FunctionType<'a>, ty: FunctionType) -> Self {
        Self { function_type, ty }
    }
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

    pub fn variables(&mut self) -> &mut HashMap<String, VariableEntry<'a>> {
        &mut self.entry.variables
    }

    pub fn functions(&mut self) -> &mut HashMap<String, FunctionEntry<'a>> {
        &mut self.entry.functions
    }

    pub fn insert_variable(&mut self, name: String, pointer: PointerValue<'a>, ty: TyKind) {
        self.entry
            .variables
            .insert(name, VariableEntry::new(pointer, ty));
    }

    pub fn insert_function(
        &mut self,
        name: String,
        llvm_function_type: types::FunctionType<'a>,
        function_type: FunctionType,
    ) {
        self.entry
            .functions
            .insert(name, FunctionEntry::new(llvm_function_type, function_type));
    }

    pub fn get_variable(&self, name: &str) -> Option<&VariableEntry<'a>> {
        match self.entry.variables.get(name) {
            Some(value) => Some(value),
            None => match &self.parent {
                Some(parent) => parent.get_variable(name),
                None => None,
            },
        }
    }

    pub fn get_function(&self, name: &str) -> Option<&FunctionEntry<'a>> {
        match self.entry.functions.get(name) {
            Some(value) => Some(value),
            None => match &self.parent {
                Some(parent) => parent.get_function(name),
                None => None,
            },
        }
    }
}
