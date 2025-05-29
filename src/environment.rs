use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::value::{LoxValue};

#[derive(Debug, PartialEq)]
pub struct Environment {
    values: HashMap<String, LoxValue>,
    enclosing: Option<Rc<RefCell<Environment>>>, // 外部作用域
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing,
        }
    }

    pub fn define(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, value); 
    }

    pub fn get(&self, name: &str) -> Result<LoxValue, ()> {
        if let Some(value) = self.values.get(name) {
            Ok(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(())
        }
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), ()> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(())
        }
    }

    pub fn check(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }
}