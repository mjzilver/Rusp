use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::value::Value;

#[derive(Debug, PartialEq)]
pub struct Env {
    store: HashMap<String, Value>,
    outer: Option<Rc<RefCell<Env>>>,
    pub output_buffer: Vec<String>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            store: HashMap::new(),
            outer: None,
            output_buffer: Vec::new(),
        }
    }

    pub fn new_child(parent: Rc<RefCell<Self>>) -> Env {
        Env {
            store: HashMap::new(),
            outer: Some(parent),
            output_buffer: Vec::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        match self.store.get(key) {
            Some(var) => Some(var.clone()),
            None => self.outer.as_ref().and_then(|o| o.borrow().get(key)),
        }
    }

    pub fn set(&mut self, key: String, value: Value) {
        self.store.insert(key, value);
    }

    pub fn add_output(&mut self, output: String) {
        if let Some(ref outer) = self.outer {
            outer.borrow_mut().add_output(output);
        } else {
            self.output_buffer.push(output);
        }
    }

    pub fn clear_output(&mut self) {
        self.output_buffer.clear();
    }

    pub fn get_output(&self) -> String {
        self.output_buffer.join("\n")
    }
}
