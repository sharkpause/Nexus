use std::collections::HashMap;

use crate::parser::{ Expression, Function, Span, Statement, TopLevel, Type };

#[derive(Debug)]
pub enum SemanticError {
    MainIsReserved {
        span: Span
    },
    DuplicateVariable {
        name: String,
        span: Span
    },
    DuplicateFunction {
        name: String,
        span: Span
    },
    DuplicateParameter { 
        name: String,
        span: Span
    },
    UndefinedVariable {
        name: String,
        span: Span
    },
    UndefinedFunction {
        name: String,
        span: Span
    },
    MismatchedArgumentCount {
        called_function_name: String,
        provided_argument_count: usize,
        expected_argument_count: usize,
        span: Span
    },
    BreakOutsideLoop {
        span: Span
    },
    ContinueOutsideLoop {
        span: Span
    },

    // ------- Fatal errors ---------
    
    NoEntryFunction,
    InvalidTopLevelStatement {
        span: Span
    },
}

impl SemanticError {
    pub fn is_fatal(&self) -> bool {
        matches!(
            self,
            SemanticError::NoEntryFunction
            | SemanticError::InvalidTopLevelStatement { .. }
        )
    }
}

pub struct Diagnostics {
    pub errors: Vec<SemanticError>
}

impl Diagnostics {
    pub fn has_fatal(&self) -> bool {
        return self.errors.iter().any(|error| error.is_fatal());
    }

    pub fn has_errors(&self) -> bool {
        return self.errors.len() > 0;
    }
}

struct FunctionSymbol {
    parameters: Vec<(Type, String)>,
    return_type: Type,
    span: Span
}

pub struct SemanticAnalyzer<'a> {
    program_tree: &'a[TopLevel],
    function_names: HashMap<String, FunctionSymbol>,
    symbol_table: Vec<HashMap<String, Span>>,
    diagnostics: Diagnostics,
    loop_depth: usize
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn from(program_tree: &'a [TopLevel]) -> Self {
        return Self {
            program_tree: program_tree,
            function_names: HashMap::new(),
            symbol_table: Vec::new(),
            diagnostics: Diagnostics { errors: Vec::new() }
        };
    }

    fn push_error(&mut self, error: SemanticError) {
        self.diagnostics.errors.push(error);
    }

    pub fn analyze(mut self) -> Diagnostics {
        self.collect_toplevels();
        if self.diagnostics.has_fatal() {
            return self.diagnostics;
        }
        
        self.validate_entry_functions();
        if self.diagnostics.has_fatal() {
            return self.diagnostics;
        }

        return self.diagnostics;
    }

    fn collect_toplevels(&mut self) {
        for toplevel in self.program_tree {
            match &toplevel {
                TopLevel::Function(function) => {
                    if let Some(existing) = self.function_names.get(&function.name) {
                        self.push_error(SemanticError::DuplicateFunction {
                            name: function.name.clone(),
                            span: function.span
                        });
                    } else {
                        self.function_names.insert(function.name.clone(),
                        FunctionSymbol {
                            parameters: function.parameters.clone(),
                            return_type: function.return_type.clone(),
                            span: function.span
                        });
                    }
                },

                TopLevel::Statement(statement) => {
                    self.push_error(SemanticError::InvalidTopLevelStatement {
                        span: statement.span(),
                    });
                }
            }
        }
    }

    fn validate_entry_functions(&mut self) {
        if !self.function_exists("entry") {
            self.push_error(SemanticError::NoEntryFunction);
        }
        
        if let Some(main_function_span) = self.function_names.get("main") {
            self.push_error(SemanticError::MainIsReserved { span: main_function_span.span });
        }
    }

    fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.symbol_table.pop();
    }

    fn add_variable(&mut self, name: String, span: Span) {
        self.symbol_table.last_mut().expect("No active scope").insert(name, span);
    }

    fn variable_exists(&mut self, name: &String, span: &Span) {
        if !self.symbol_table.last().expect("No active scope").contains_key(name) {
            self.push_error(SemanticError::UndefinedVariable {
                name: name.clone(),
                span: span.clone(),
            });
        }
    }

    fn function_exists(&mut self, name: &str) -> bool {
        return self.function_names.contains_key(name);
    }

    fn lookup_function(&self, name: &str) -> Option<&FunctionSymbol> {
        return self.function_names.get(name);
    }

    // Make a function_exists method to check if a function exists or not, take the logic from validate_entry_functions,
    // Delete validate_entry_functions and call function_exists twice to check for main and entry

    fn validate_tree(&mut self) {
        for toplevel in self.program_tree {
            match toplevel {
                TopLevel::Function(function) => {
                    self.validate_function(function);
                },
                _ => unreachable!("Invalid top-level nodes should have been caught earlier")
            }
        }
    }

    fn validate_function(&mut self, function: &Function) {
        self.enter_scope();

        for (_type, name) in &function.parameters {
            self.add_variable(name.clone(), function.span);
        }

        self.validate_statement(&function.body);

        self.exit_scope();
    }

    fn validate_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Return { value, span } => {
                if let Some(value_expression) = value {
                    self.validate_expression(value_expression);
                }
            },

            Statement::Block { statements, span } => {
                self.enter_scope();

                for statement in statements {
                    self.validate_statement(statement);
                }

                self.exit_scope();
            },
            
            Statement::Break { span } => {
                if self.loop_depth == 0 {
                    self.push_error(SemanticError::BreakOutsideLoop { span: *span });
                }
            },

            Statement::Continue { span } => {
                if self.loop_depth == 0 {
                    self.push_error(SemanticError::ContinueOutsideLoop { span: *span });
                }
            },

            Statement::Expression { expression, span } => {
                self.validate_expression(expression);
            }
        }
    }

    fn validate_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Variable { name, span } => {
                self.variable_exists(name, span);
            },

            Expression::FunctionCall { called, arguments, span } => {
                let Expression::Variable { name: called_function_name, span: function_span } = called.as_ref()
                    else { unreachable!("Parser guarantees called is a variable") };
                
                if let Some(called_function) = self.lookup_function(called_function_name) {
                    if called_function.parameters.len() != arguments.len() {
                        self.push_error(SemanticError::MismatchedArgumentCount {
                            called_function_name: called_function_name.clone(),
                            provided_argument_count: arguments.len(),
                            expected_argument_count: called_function.parameters.len(),
                            span: *span,
                        });
                    }
                } else {
                    self.push_error(SemanticError::UndefinedFunction {
                        name: called_function_name.clone(),
                        span: *function_span,
                    });
                }

                for argument in arguments {
                    self.validate_expression(argument);
                }
            },

            Expression::BinaryOperation { left, operator, right, span } => {
                self.validate_expression(left);
                self.validate_expression(right);
            },

            Expression::UnaryOperation { operator, operand, span } => {
                self.validate_expression(operand);
            },

            Expression::IntLiteral { value, span } => {
                // yes
            }
        }
    }
}