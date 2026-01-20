use std::{collections::HashMap, env::var};

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
    MismatchedReturnType {
        expected_return_type: Type,
        provided_return_type: Type,
        span: Span
    },
    MismatchedVariableType {
        name: String,
        expected_type: Type,
        provided_type: Type,
        span: Span  
    },
    MismatchedBinaryOperationType {
        left_type: Type,
        right_type: Type,
        span: Span
    },
    MissingReturnType {
        expected: Type,
        span: Span
    },
    InvalidType {
        var_name: String,
        var_type: Type,
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

#[derive(Debug)]
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

struct VariableSymbol {
    var_type: Type,
    span: Span
}

pub struct SemanticAnalyzer<'a> {
    program_tree: &'a mut [TopLevel],
    function_names: HashMap<String, FunctionSymbol>,
    symbol_table: Vec<HashMap<String, VariableSymbol>>,
    diagnostics: Diagnostics,
    loop_depth: usize,
    current_return_type: Type
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn from(program_tree: &'a mut [TopLevel]) -> Self {
        return Self {
            program_tree: program_tree,
            function_names: HashMap::new(),
            symbol_table: Vec::new(),
            diagnostics: Diagnostics { errors: Vec::new() },
            loop_depth: 0,
            current_return_type: Type::Int64
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
            return self.diagnostics
        }

        if self.diagnostics.has_fatal() {
            return self.diagnostics;
        }

        self.validate_tree();

        self.diagnostics
    }

    fn collect_toplevels(&mut self) {
        let mut errors: Vec<SemanticError> = Vec::new();

        for toplevel in self.program_tree.iter_mut() {
            match toplevel {
                TopLevel::Function(function) => {
                    if let Some(existing) = self.function_names.get(&function.name) {
                        errors.push(SemanticError::DuplicateFunction {
                            name: function.name.clone(),
                            span: function.span
                        });
                    } else {
                        if function.name == "main" {
                            errors.push(SemanticError::MainIsReserved { span: function.span });
                        } else if function.name == "entry" {
                            function.name = "main".to_string();
                        }

                        self.function_names.insert(function.name.clone(),
                        FunctionSymbol {
                            parameters: function.parameters.clone(),
                            return_type: function.return_type.clone(),
                            span: function.span
                        });
                    }
                },

                TopLevel::Statement(statement) => {
                    errors.push(SemanticError::InvalidTopLevelStatement {
                        span: statement.span(),
                    });
                }
            }
        }

        for error in errors {
            self.push_error(error);
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

    fn add_variable(&mut self, name: String, type_: Type, span: Span) {
        let scope = self.symbol_table.last_mut().expect("No active scope");

        if scope.contains_key(&name) {
            self.push_error(SemanticError::DuplicateVariable { name, span });
        } else {
            scope.insert(name, VariableSymbol { var_type: type_, span });
        }
    }

    fn variable_exists(&mut self, name: &String, span: &Span) {
        for scope in self.symbol_table.iter().rev() {
            if scope.contains_key(name) {
                return;
            }
        }

        self.push_error(SemanticError::UndefinedVariable {
            name: name.clone(),
            span: span.clone(),
        });
    }

    fn lookup_variable(&self, name: &String) -> Option<&VariableSymbol> {
        return self.symbol_table.last().expect("Variable should exist at this point").get(name);
    }

    fn function_exists(&self, name: &str) -> bool {
        return self.function_names.contains_key(name);
    }

    fn lookup_function(&self, name: &str) -> Option<&FunctionSymbol> {
        return self.function_names.get(name);
    }

    fn validate_tree(&mut self) {
        let mut program = std::mem::take(&mut self.program_tree);
        // this is needed because this it needs to iterate through self.program_tree, so
        // two errors can occur: A multiple mutable borrow and a mutable borrow after an immutable borrow
        // this fixes that error because self.program_tree now is moved, no need for a borrow
        // fuck you borrow checker

        for toplevel in program.iter_mut() {
            if let TopLevel::Function(function) = toplevel {
                self.validate_function(function);
            }
        }
        
        self.program_tree = program;
    }

    fn validate_function(&mut self, function: &Function) {
        self.current_return_type = function.return_type.clone();

        self.enter_scope();

        for (_type, name) in &function.parameters {
            self.add_variable(name.clone(), function.return_type.clone(), function.span);
        }

        if let Statement::Block { statements, .. } = &function.body {
            for statement in statements {
                self.validate_statement(statement);
            }
        } else {
            unreachable!("Function body should always be a block");
        }

        self.exit_scope();
    }

    fn validate_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Return { value, span } => {
                self.validate_expression(value);
                
                if let Some(value_type) = self.infer_expression_type(value) {
                    if !value_type.same_kind(&self.current_return_type) {
                        self.push_error(SemanticError::MismatchedReturnType {
                            expected_return_type: self.current_return_type.clone(),
                            provided_return_type: value_type,
                            span: *span
                        });
                    }
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
            },

            Statement::If { condition, then_branch, else_branch, span } => {
                self.validate_expression(condition);
                self.validate_statement(then_branch);
                
                if let Some(else_body) = else_branch {
                    self.validate_statement(else_body);
                }
            },

            Statement::VariableAssignment { name, value, span } => {
                self.validate_expression(value);
                        
                if let Some(value_type) = self.infer_expression_type(value) {
                    if let Some(var_symbol) = self.lookup_variable(name) {
                        if var_symbol.var_type.is_void() {
                            self.push_error(SemanticError::InvalidType {
                                var_name: name.clone(),
                                var_type: Type::Void,
                                span: *span
                            });
                        } else if !var_symbol.var_type.same_kind(&value_type) {
                            self.push_error(SemanticError::MismatchedVariableType {
                                name: name.clone(),
                                expected_type: var_symbol.var_type.clone(),
                                provided_type: value_type,
                                span: *span,
                            });
                        }
                    }
                }
            }

            Statement::VariableDeclare { var_type, name, initializer, span } => {
                self.validate_expression(initializer);

                if let Some(init_type) = self.infer_expression_type(initializer) {
                    if init_type.is_void() {
                        self.push_error(SemanticError::InvalidType {
                            var_name: name.clone(),
                            var_type: Type::Void,
                            span: *span
                        });
                    } else if !var_type.same_kind(&init_type) {
                        self.push_error(SemanticError::MismatchedVariableType {
                            name: name.clone(),
                            expected_type: var_type.clone(),
                            provided_type: init_type,
                            span: *span,
                        });
                    }

                    self.add_variable(name.clone(), var_type.clone(), *span);
                }
            },

            Statement::While { condition, body, span } => {
                self.loop_depth += 1;

                self.validate_expression(condition);
                self.validate_statement(body);
            
                self.loop_depth -= 1;
            }
        }
    }

    fn infer_expression_type(&mut self, expression: &Expression) -> Option<Type> {
        match expression {
            Expression::IntLiteral64 { value, span } => {
                return Some(Type::Int64);
            },

            Expression::Null { span } => {
                return Some(Type::Void)
            }

            Expression::FunctionCall { called, arguments, span } => {
                let Expression::Variable { name: called_function_name, span: function_span } = called.as_ref()
                    else { unreachable!("Parser guarantees called is a variable") };
                
                return Some(self.lookup_function(called_function_name)?.return_type.clone());
            },

            Expression::Variable { name, span } => {
                return Some(self.lookup_variable(name)?.var_type.clone());
            },

            Expression::UnaryOperation { operator, operand, span } => {
                return self.infer_expression_type(operand);
                // In the future check if the operand can logically be negated
            },

            Expression::BinaryOperation { left, operator, right, span } => {
                let left_type = self.infer_expression_type(left)?;
                let right_type = self.infer_expression_type(right)?;

                if left_type.same_kind(&right_type) {
                    return Some(left_type);
                } else {
                    self.push_error(SemanticError::MismatchedBinaryOperationType { left_type, right_type, span: *span });
                    return None;
                }
            },
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

            Expression::IntLiteral64 { value, span } => {
                // yes
            },

            Expression::Null { span } => {
                // yes
            }
        }
    }
}