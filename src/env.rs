use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::Object;

#[derive(Debug, PartialEq)]
pub struct Env {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        let mut env = Env {
            store: HashMap::new(),
            outer: None,
        };
        env.set("T".to_string(), Object::Bool(true));
        env.set("NIL".to_string(), Object::Bool(false));
        env
    }

    pub fn new_child(parent: Rc<RefCell<Self>>) -> Env {
        Env {
            store: HashMap::new(),
            outer: Some(parent),
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(var) => Some(var.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|o| o.borrow().get(key).clone()),
        }
    }

    pub fn set(&mut self, key: String, value: Object) {
        self.store.insert(key, value);
    }
}
