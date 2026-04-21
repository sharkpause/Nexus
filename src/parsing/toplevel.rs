use crate::parsing::{function::Function, statement::Statement};

#[derive(Debug, Clone)]
pub enum TopLevel {
    Function(Function),

    Statement(Statement), // All this does is cause a semantic error
}