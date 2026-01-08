use std::{ collections::HashMap };

use crate::parser::{Expression, Operator, Statement, TopLevel, Type};

#[derive(Debug)]
pub enum CodegenError {
    GenericError,
    UndefinedVariable(String),
    InvalidBreak,
    InvalidContinue
}

pub trait Backend {
    fn generate(&mut self, progrma: Vec<TopLevel>) -> Result<String, CodegenError>;
}