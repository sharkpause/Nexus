use crate::parsing::{span::Span, statement::Statement, types::Type};

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<(Type, String)>,
    pub body: Option<Statement>,
    pub span: Span,
}