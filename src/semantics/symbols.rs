use crate::parsing::{span::Span, types::Type};

pub struct FunctionSymbol {
    pub parameters: Vec<(Type, String)>,
    pub return_type: Type,
    pub span: Span
}

#[derive(Debug)]
pub struct VariableSymbol {
    pub type_: Type,
    pub span: Span
}
