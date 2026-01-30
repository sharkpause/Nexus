use crate::parser::{ TopLevel };

#[derive(Debug)]
pub enum CodegenError {
    GenericError,
    UndefinedVariable(String),
    InvalidBreak,
    InvalidContinue,
    UnknownExpression,
    UnknownStatement,
    InvalidType,
}

pub trait Backend {
    fn generate(&mut self, program: Vec<TopLevel>) -> Result<String, CodegenError>;
}

pub fn generate_program(program: Vec<TopLevel>, backend: &mut dyn Backend) -> Result<String, CodegenError> {
    return backend.generate(program);
}