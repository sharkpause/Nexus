use std::collections::HashMap;

use crate::parsing::types::Type;
use crate::{
    errors::SemanticError,
    parsing::{expression::Expression, operator::Operator},
    semantics::{
        diagnostics::Diagnostics,
        symbols::{FunctionSymbol, VariableSymbol},
    },
};

pub struct SemanticContext {
    pub symbol_table: Vec<HashMap<String, VariableSymbol>>,
    pub function_names: HashMap<String, FunctionSymbol>,
    pub diagnostics: Diagnostics,
    pub loop_depth: usize,
    pub current_return_type: Type,
}

impl SemanticContext {
    pub fn new() -> Self {
        return Self {
            function_names: HashMap::new(),
            symbol_table: Vec::new(),
            diagnostics: Diagnostics { errors: Vec::new() },
            loop_depth: 0,
            current_return_type: Type::Int64,
        };
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&VariableSymbol> {
        for scope in self.symbol_table.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }

    pub fn lookup_variable_in_current_scope(&self, name: &str) -> Option<&VariableSymbol> {
        let v = self.symbol_table.last().and_then(|scope| scope.get(name));
        println!("{:?}", v.clone());
        return v;
    }

    pub fn lookup_variable_type(&self, name: &str) -> Option<Type> {
        return self.lookup_variable(name).map(|v| v.type_.clone());
    }

    pub fn unify_binary_types(&self, left: &Type, right: &Type, _operator: &Operator) -> Type {
        if left.same_kind(right) {
            return left.clone();
        }

        if matches!(left, Type::Null) && right.is_pointer() {
            return right.clone();
        }

        if matches!(right, Type::Null) && left.is_pointer() {
            return left.clone();
        }

        if !left.is_generic() && right.is_generic() {
            return left.clone();
        }

        if left.is_generic() && !right.is_generic() {
            return right.clone();
        }

        // TODO: Default to specific data type:
        // int -> int32        float -> float32
        if left.is_generic() && right.is_generic() {
            return Type::Int32;
        }

        panic!("invalid binary type unification: {:?} {:?}", left, right)
    }

    pub fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.symbol_table.pop();
    }

    pub fn insert_variable(&mut self, name: String, symbol: VariableSymbol) {
        self.symbol_table.last_mut().unwrap().insert(name, symbol);
    }

    pub fn push_error(&mut self, error: SemanticError) {
        self.diagnostics.errors.push(error);
    }
}
