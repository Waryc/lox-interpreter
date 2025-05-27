use std::rc::Rc;
use std::cell::RefCell;

use crate::environment::Environment;
use crate::{error::RuntimeError, report_error};
use crate::value::LoxValue;

pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>, // 全局环境
    pub environment: Rc<RefCell<Environment>>, // 当前环境（可能指向globals）
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));
        Self {
            globals: globals.clone(),
            environment: globals, 
        }
    }
}