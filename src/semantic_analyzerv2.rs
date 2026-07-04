use std::collections::HashMap;

use crate::{errors::SemanticError, parsing::{function::{self, Function}, statement::Statement, toplevel::TopLevel, types::Type}, semantics::{diagnostics::Diagnostics, semantic_context::SemanticContext, symbols::{FunctionSymbol, VariableSymbol}}};

pub struct SemanticAnalyzer {
    pub program_tree: Vec<TopLevel>,
    pub context: SemanticContext
}

impl SemanticAnalyzer {
    pub fn from(program_tree: Vec<TopLevel>) -> Self {
        return Self {
            program_tree: program_tree,
            context: SemanticContext::new()
        };
    }
    

    pub fn analyze(&mut self) -> Vec<TopLevel> {
        let mut program_tree = std::mem::take(&mut self.program_tree);

        for toplevel in &mut program_tree {
            self.register_toplevel(toplevel);
        }

        for toplevel in &mut program_tree {
            self.analyze_toplevel(toplevel);
        }

        return program_tree;
    }

    fn register_toplevel(&mut self, toplevel: &mut TopLevel) {
        match toplevel {
            TopLevel::Function(function)=> {
                if function.name == "main" {
                    self.context.push_error(
                        SemanticError::MainIsReserved { span:function.span }
                    );

                    return;
                } else if function.name == "entry" && !function.return_type .same_kind(&Type::Int32) {
                    self.context.push_error(
                        SemanticError::InvalidEntryReturnType { span: function.span }
                    );
                }
                
                let symbol = FunctionSymbol {
                    parameters: function.parameters.clone(),
                    return_type: function.return_type.clone(),
                    span: function.span
                };

                self.context.insert_function(function.name.clone(), symbol);
            },

            TopLevel::Statement(..) => {
                unimplemented!("Top level statements aren't supported");
            }
        }
    }

    fn analyze_toplevel(&mut self, toplevel: &mut TopLevel) {
        match toplevel {
            TopLevel::Function(function) => {
                self.analyze_function(function);
            },
            TopLevel::Statement(statement) => {
                self.context.push_error(
                    SemanticError::InvalidTopLevelStatement { span: statement.span() }
                );
            }
        }
    }

    fn analyze_function(&mut self, function: &mut Function) {
        let previous_return_type = self.context.current_return_type.clone();
        self.context.current_return_type = function.return_type.clone();

        self.context.enter_scope();

        for(parameter_type, name) in &function.parameters {
            let symbol = VariableSymbol {
                type_: parameter_type.clone(),
                span: function.span
            };

            self.context.insert_variable(name.clone(), symbol);
        }

        if let Some(function_body) = &mut function.body {
            function_body.validate(&mut self.context);
        }

        self.context.exit_scope();
        self.context.current_return_type = previous_return_type;
    }
}