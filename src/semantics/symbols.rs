use crate::parsing::{span::Span, types::Type};

pub struct FunctionSymbol {
    pub parameters: Vec<(Type, String)>,
    pub return_type: Type,
    pub span: Span
}

pub struct VariableSymbol {
    pub var_type: Type,
    pub span: Span
}
