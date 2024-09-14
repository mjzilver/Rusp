use std::collections::HashMap;

use crate::parser::Object;

pub struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Object> {
        self.store.get(key)
    }
    
    pub fn set(&mut self, key: String, value: Object) {
        self.store.insert(key, value);
    }
}
