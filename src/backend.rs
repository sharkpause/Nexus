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
    fn generate(&mut self, program: Vec<TopLevel>) -> Result<String, CodegenError>;
}

pub fn generate_program(program: Vec<TopLevel>, backend: &mut dyn Backend) -> Result<String, CodegenError> {
    return backend.generate(program);
}