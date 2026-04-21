use std::collections::HashMap;

use crate::semantics::{diagnostics::Diagnostics, symbols::{FunctionSymbol, VariableSymbol}};
use crate::parsing::types::Type;

pub struct SemanticContext {
    symbol_table: Vec<HashMap<String, VariableSymbol>>,
    function_names: HashMap<String, FunctionSymbol>,
    diagnostics: Diagnostics,
    loop_depth: usize,
    current_return_type: Type,
}