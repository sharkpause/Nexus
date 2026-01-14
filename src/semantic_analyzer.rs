use std::collections::HashSet;

use crate::parser::{ TopLevel };

#[derive(Debug)]
pub enum SemanticError {
    NoEntryFunction,
    MainIsReserved,
    DuplicateVariable {
        name: String
    },
    DuplicateFunction {
        name: String
    },
    DuplicateParameter{ 
        name: String
    },
    UndefinedVariable{
        name: String
    },
}

pub struct SemanticAnalyzer {
    program_tree: Vec<TopLevel>,
    function_names: HashSet<String>
}

impl SemanticAnalyzer {
    pub fn from(program_tree: &Vec<TopLevel>) -> Self {
        // Potentially change to take ownership of program_tree instead of copy for performance and memory usage
        return Self {
            program_tree: program_tree.to_vec(),
            function_names: HashSet::new()
        };
    }

    pub fn analyze(&mut self) -> Result<(), SemanticError> {
        self.collect_functions();
        self.validate_entry_functions()?;
        // TODO: Make minimal semantic check for no entry function and main is reserved
    
        return Ok(());
    }

    fn collect_functions(&mut self) {
        for toplevel in &self.program_tree {
            if let TopLevel::Function(function) = toplevel {
                self.function_names.insert(function.name.clone());
            }
        }
    }

    fn validate_entry_functions(&self) -> Result<(), SemanticError> {
        if !self.function_names.contains("entry") {
            return Err(SemanticError::NoEntryFunction);
        }
        
        if self.function_names.contains("main") {
            return Err(SemanticError::MainIsReserved);
        }

        return Ok(());
    }
}