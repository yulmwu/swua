use super::value::{Type, Value};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    pub entries: HashMap<String, EnvironmentEntry>,
    pub parent: Option<Box<Environment>>,
}

#[derive(Debug, Clone)]
pub struct EnvironmentEntry {
    pub value: Value,
    pub ty: Type,
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

    pub fn insert(&mut self, name: String, value: Value, ty: Type) -> Option<EnvironmentEntry> {
        self.entries.insert(name, EnvironmentEntry { value, ty })
    }

    pub fn get(&self, name: String) -> Option<&EnvironmentEntry> {
        match self.entries.get(&name) {
            Some(entry) => Some(entry),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }
}
