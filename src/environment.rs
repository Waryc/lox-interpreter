use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::value::{LoxValue};

#[derive(Debug, Clone)]
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
        self.values.get(name)
            .map(|v| v.clone())
            .or_else(|| self.enclosing.as_ref().and_then(|e| e.borrow().get(name).ok()))
            .ok_or(())
    }

    pub fn enter(&self) -> Environment {
        Environment::new(Some(Rc::new(RefCell::new(self.clone()))))
    }
}