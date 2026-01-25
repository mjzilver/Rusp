use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::Object;

#[derive(Debug, PartialEq)]
pub struct Env {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Env>>>,
    pub output_buffer: Vec<String>,
}

impl Env {
    pub fn new() -> Self {
        let mut env = Env {
            store: HashMap::new(),
            outer: None,
            output_buffer: Vec::new(),
        };
        env.set("T".to_string(), Object::Bool(true));
        env.set("NIL".to_string(), Object::Bool(false));
        env
    }

    pub fn new_child(parent: Rc<RefCell<Self>>) -> Env {
        Env {
            store: HashMap::new(),
            outer: Some(parent),
            output_buffer: Vec::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(var) => Some(var.clone()),
            None => self.outer.as_ref().and_then(|o| o.borrow_mut().get(key)),
        }
    }

    pub fn set(&mut self, key: String, value: Object) {
        self.store.insert(key, value);
    }

    pub fn add_output(&mut self, output: String) {
        self.output_buffer.push(output);
    }

    #[allow(dead_code)]
    pub fn clear_output(&mut self) {
        self.output_buffer.clear();
    }

    #[allow(dead_code)]
    pub fn get_output(&self) -> String {
        self.output_buffer.join("\n")
    }
}
