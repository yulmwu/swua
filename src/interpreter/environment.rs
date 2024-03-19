use super::value::{Type, Value};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    pub variables: HashMap<String, VariableEntry>,
    pub structs: HashMap<String, StructEntry>,
    pub parent: Option<Box<Environment>>,
}

#[derive(Debug, Clone)]
pub struct VariableEntry {
    pub value: Value,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct StructEntry {
    pub fields: HashMap<String, Type>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with_parent(parent: Environment) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            ..Default::default()
        }
    }

    pub fn insert(&mut self, name: String, value: Value, ty: Type) -> Option<VariableEntry> {
        self.variables.insert(name, VariableEntry { value, ty })
    }

    pub fn get(&self, name: String) -> Option<&VariableEntry> {
        match self.variables.get(&name) {
            Some(entry) => Some(entry),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }
}
