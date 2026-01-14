use std::collections::HashMap;

use crate::parser::{ TopLevel, Expression, Function, Span };

#[derive(Debug)]
pub enum SemanticError {
    NoEntryFunction,
    MainIsReserved {
        function: Function
    },
    DuplicateVariable {
        variable: Expression
    },
    DuplicateFunction {
        function: Function
    },
    DuplicateParameter{ 
        variable: Expression
    },
    UndefinedVariable{
        variable: Expression
    },
}

pub struct SemanticAnalyzer {
    program_tree: Vec<TopLevel>,
    function_names: HashMap<String, Function>
}

impl SemanticAnalyzer {
    pub fn from(program_tree: &Vec<TopLevel>) -> Self {
        // Potentially change to take ownership of program_tree instead of copy for performance and memory usage
        return Self {
            program_tree: program_tree.to_vec(),
            function_names: HashMap::new()
        };
    }

    pub fn analyze(&mut self) -> Result<(), SemanticError> {
        self.collect_functions();
        self.validate_entry_functions()?;
    
        return Ok(());
    }

    fn collect_functions(&mut self) {
        for toplevel in &self.program_tree {
            if let TopLevel::Function(function) = toplevel {
                self.function_names.insert(function.name.clone(), function.clone());
            }
        }
    }

    fn validate_entry_functions(&self) -> Result<(), SemanticError> {
        if !self.function_names.contains_key("entry") {
            return Err(SemanticError::NoEntryFunction);
        }
        
        if let Some(main_function) = self.function_names.get("main") {
            return Err(SemanticError::MainIsReserved { function: main_function.clone() });
        }

        return Ok(());
    }
}