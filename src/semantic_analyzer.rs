use std::{collections::HashMap, env::var, slice::GetDisjointMutError};

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
    InvalidEntryReturnType {
        span: Span
    },
    IntegerOverflow {
        span: Span
    },
    MismatchedArgumentType {
        expected_type: Type,
        provided_type: Type,
        span: Span
    },
    MismatchedAssignmentType {
        expected_type: Type,
        provided_type: Type,
        span: Span
    },
    InvalidTypeWidening {
        from_type: Type,
        to_type: Type,
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
    current_return_type: Type,
    generics: Vec<Type>
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn from(program_tree: &'a mut [TopLevel]) -> Self {
        return Self {
            program_tree: program_tree,
            function_names: HashMap::new(),
            symbol_table: Vec::new(),
            diagnostics: Diagnostics { errors: Vec::new() },
            loop_depth: 0,
            current_return_type: Type::Int64,
            generics: Vec::new()
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

    fn variable_exists(&mut self, name: &String, span: &Span) -> bool {
        for scope in self.symbol_table.iter().rev() {
            if scope.contains_key(name) {
                return false;
            }
        }

        self.push_error(SemanticError::UndefinedVariable {
            name: name.clone(),
            span: span.clone(),
        });
        return true;
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
        // this fixes that error because self.program_tree now is moved, no need for a borrow.
        // fuck you borrow checker.

        for toplevel in program.iter_mut() {
            if let TopLevel::Function(function) = toplevel {
                self.validate_function(function);
            }
        }
        
        self.program_tree = program;
    }

    fn validate_function(&mut self, function: &mut Function) {
        self.current_return_type = function.return_type.clone();

        if function.name == "main" && !self.current_return_type.same_kind(&Type::Int32) {
            self.push_error(SemanticError::InvalidEntryReturnType { span: function.span });
        }

        self.enter_scope();

        for (parameter_type, name) in &function.parameters {
            self.add_variable(name.clone(), parameter_type.clone(), function.span);
        }

        if let Statement::Block { statements, .. } = &mut function.body {
            for statement in statements {
                self.validate_statement(statement);
            }
        } else {
            unreachable!("Function body should always be a block");
        }

        self.exit_scope();
    }

    fn validate_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Return { value, span } => {
                let return_type = self.current_return_type.clone();
                
                // We use raw pointers because "generics" needs to hold mutable references to the expressions
                // if we used &mut Expresion because I'm pretty sure the borrow checker,
                // even after validate_expression is done with the mutable references to the expressions
                // still treats the expressions as still being borrowed even though no additional operations
                // are being done on the expressions after validate_expression. So a raw pointer is needed
                // to bypass the borrow checker and mutate the expressions.
                let mut generics: Vec<*mut Expression> = Vec::new();

                let mut provided_type = if let Some(expression) = value {
                    match self.validate_expression(expression, &mut Some(&mut generics)) {
                        Ok(t) => t,
                        Err(_) => return, // error already pushed
                    }
                } else {
                    Type::Void
                };

                for generic_typed_expression_pointer in generics {
                    let generic_typed_expression: &mut Expression = unsafe { &mut *generic_typed_expression_pointer };
                    // This is safe, expression is not mutated anymore after validate_expression.
                    // No race condition will happen because validate_expression does not do any additional operations
                    // on the expressions inside generics.

                    provided_type = match self.cast_to_default_types(generic_typed_expression, &return_type) {
                        Ok(t) => t,
                        Err(_) => return, // error already pushed
                    }
                }

                if !provided_type.same_kind(&return_type) {
                    self.push_error(SemanticError::MismatchedReturnType {
                        expected_return_type: return_type,
                        provided_return_type: provided_type,
                        span: *span,
                    });
                    return;
                }

                // if let Some(expr) = value {
                //     self.widen_expression(expr, &return_type);
                // }

                if let Some(expr) = value {
                    self.validate_expression(expr, &mut None);
                }
            }

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
                self.validate_expression(expression, &mut None);
            },

            Statement::If { condition, then_branch, else_branch, span } => {
                self.validate_expression(condition, &mut None);
                self.validate_statement(then_branch);
                
                if let Some(else_body) = else_branch {
                    self.validate_statement(else_body);
                }
            },

            Statement::VariableAssignment { name, value, span } => {
                self.validate_expression(value, &mut None);
                        
                if let Some(value_type) = self.infer_expression_type(value) {
                    if let Some(var_symbol) = self.lookup_variable(name) {
                        if var_symbol.var_type.is_void() {
                            self.push_error(SemanticError::InvalidType {
                                var_name: name.clone(),
                                var_type: Type::Void,
                                span: *span
                            });
                        } else if !var_symbol.var_type.is_assignable_to(&value_type) {
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
                self.validate_expression(initializer, &mut None);

                if let Some(init_type) = self.infer_expression_type(initializer) {
                    if init_type.is_void() {
                        self.push_error(SemanticError::InvalidType {
                            var_name: name.clone(),
                            var_type: Type::Void,
                            span: *span
                        });
                    } else if !var_type.is_assignable_to(&init_type) {
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

                self.validate_expression(condition, &mut None);
                self.validate_statement(body);
            
                self.loop_depth -= 1;
            }
        }
    }

    fn infer_expression_type(&mut self, expression: &mut Expression) -> Option<Type> {
        match expression {
            Expression::IntLiteral { value, span } => {
                if *value < i128::MIN || *value > i128::MAX {
                    self.push_error(SemanticError::IntegerOverflow { span: *span });

                    return None;
                }

                return Some(Type::GenericInt);
            },

            Expression::FunctionCall { called, arguments, span } => {
                let Expression::Variable { name: called_function_name, type_, span: function_span } = called.as_ref()
                    else { unreachable!("Parser guarantees called is a variable") };
                
                return Some(self.lookup_function(called_function_name)?.return_type.clone());
            },

            Expression::Variable { name, type_, span } => {
                return Some(self.lookup_variable(name)?.var_type.clone());
            },

            Expression::UnaryOperation { operator, operand, span } => {
                return self.infer_expression_type(operand);
                // In the future check if the operand can logically be negated
            },

            Expression::BinaryOperation { left, operator, right, span } => {
                let mut left_type = self.infer_expression_type(left)?;
                let mut right_type = self.infer_expression_type(right)?;

                // Handle GenericInt adoption, need to change this later
                // Otherwise it'd look ugly when I have GenericFloat, etc
                if left_type.same_kind(&Type::GenericInt) && !right_type.same_kind(&Type::GenericInt) {
                    self.widen_expression(left, &right_type);
                    left_type = right_type.clone();
                } else if right_type.same_kind(&Type::GenericInt) && !left_type.same_kind(&Type::GenericInt) {
                    self.widen_expression(right, &left_type);
                    right_type = left_type.clone();
                }

                if left_type.same_kind(&right_type) {
                // if left_type.is_assignable_to(&right_type) {
                    return Some(left_type);
                } else {
                    self.push_error(SemanticError::MismatchedBinaryOperationType { left_type, right_type, span: *span });
                    return None;
                }
            },

            Expression::IntLiteral32 { value, span } => {
                return Some(Type::Int32);
            },
            
            Expression::IntLiteral64 { value, span } => {
                return Some(Type::Int64);
            }
        }
    }

    fn validate_expression(
        &mut self,
        expression: & mut Expression,
        generics: &mut Option<& mut Vec<*mut Expression>>
    ) -> Result<Type, ()> {
        match expression {
            Expression::Variable { name, type_, span } => {
                if self.variable_exists(name, span) {
                    return Err(());
                }

                if let Some(var_type) = type_ {
                    return Ok(var_type.clone());
                }
                
                let var_type =
                    self.lookup_variable(name)
                        .expect("Variable is guaranteed to exist")
                        .var_type
                        .clone();

                *type_ = Some(var_type);
                return Ok(type_.clone().expect("Type is guaranteed here"));
            },

            Expression::FunctionCall { .. } => {
                // A match with no borrows up there and then a borrow down there is needed to prevent
                // two mutable borrows of expression

                let inferred_type = self.infer_expression_type(expression);
                
                match expression {
                    Expression::FunctionCall { called, arguments, span } => {
                        let Expression::Variable { name: called_function_name, type_: called_type, span: function_span } =
                            called.as_mut()
                            else { unreachable!("Parser guarantees called is a variable") };

                        let called_function = match self.lookup_function(called_function_name) {
                            Some(f) => f,
                            None => {
                                self.push_error(SemanticError::UndefinedFunction {
                                    name: called_function_name.clone(),
                                    span: *function_span,
                                });
                                return Err(());
                            }
                        };

                        if called_function.parameters.len() != arguments.len() {
                            self.push_error(SemanticError::MismatchedArgumentCount {
                                called_function_name: called_function_name.clone(),
                                provided_argument_count: arguments.len(),
                                expected_argument_count: called_function.parameters.len(),
                                span: *span,
                            });
                            return Err(());
                        }

                        let expected_params = called_function.parameters.clone();
                        for (provided_argument, expected_argument) in
                            arguments.iter_mut().zip(expected_params.iter())
                        {
                            self.validate_expression(provided_argument, generics)?;
                            self.validate_argument(provided_argument, expected_argument);
                        }

                        *called_type = inferred_type;

                        return Ok(called_type.clone().expect("Type is guaranteed here"));
                    },
                    _ => unreachable!()
                }
            },

            Expression::BinaryOperation { .. } => {
                let expression_type = self.infer_expression_type(expression);

                match expression {
                    Expression::BinaryOperation { left, operator, right, span } => {
                        self.validate_expression(left, generics)?;
                        self.validate_expression(right, generics)?;
                    },
                    _ => unreachable!()
                }

                // match expression {
                //     Expression::BinaryOperation { .. } => {
                        
                //         return Err(());
                //     },
                //     _ => unreachable!()
                // }

                if let Some(some_type) = expression_type {
                    return Ok(some_type);
                }
                return Err(());
            }

            Expression::UnaryOperation { operator, operand, span } => {
                return self.validate_expression(operand, generics);
            },

            Expression::IntLiteral { value, span } => {
                if let Some(some_generics) = generics {
                    some_generics.push(expression);
                }

                return Ok(Type::GenericInt);
            },

            Expression::IntLiteral32 { value, span } => {
                return Ok(Type::Int32);
            },

            Expression::IntLiteral64 { value, span } => {
                return Ok(Type::Int64);
            },
        }
    }

    fn validate_argument(&mut self, provided_argument: &mut Expression, expected_argument: &(Type, String)) -> Result<(), ()> {
        match provided_argument {
            Expression::IntLiteral { value, span } => {
                if !expected_argument.0.is_assignable_to(&Type::GenericInt) {
                    self.push_error(SemanticError::MismatchedArgumentType {
                        expected_type: expected_argument.0.clone(),
                        provided_type: Type::GenericInt,
                        span: *span
                    });
                    return Err(());
                }

                *provided_argument = match &expected_argument.0 {
                    Type::Int32 => Expression::IntLiteral32 { value: *value as i32, span: *span },
                    Type::Int64 => Expression::IntLiteral64 { value: *value as i64, span: *span },
                    _ => unreachable!("Should only be integer types here")
                };
            },

            Expression::Variable { name, type_, span } => {
                let var_type = type_.as_ref().expect("Type should be guaranteed here");
                
                if !expected_argument.0.is_assignable_to(&var_type) {
                    self.push_error(SemanticError::MismatchedArgumentType {
                        expected_type: expected_argument.0.clone(),
                        provided_type: var_type.clone(),
                        span: *span
                    });
                    return Err(());
                }
            },

            Expression::BinaryOperation { left, operator, right, span } => {
                self.validate_argument(left, expected_argument)?;
                self.validate_argument(right, expected_argument)?;
            },

            Expression::FunctionCall { called, arguments, span } => {
                let called_type = {
                    let Expression::Variable { type_: called_type, .. } =
                        called.as_ref()
                    else {
                        unreachable!("Parser guarantees called is a variable");
                    };

                    called_type.clone()
                };
                
                if let Some(ref some_called_type) = called_type {
                    if !some_called_type.is_assignable_to(&expected_argument.0) {
                        self.push_error(SemanticError::MismatchedArgumentType {
                            expected_type: expected_argument.0.clone(),
                            provided_type: some_called_type.clone(),
                            span: *span
                        });
                        return Err(());
                    }
                }

                self.validate_expression(provided_argument, &mut None)?;
            },

            Expression::UnaryOperation { operator, operand, span } => {
                self.validate_argument(operand, expected_argument)?;
            },

            Expression::IntLiteral32 { value, span } => {
                match &expected_argument.0 {
                    Type::Int64 => {
                        *provided_argument = Expression::IntLiteral64 { value: *value as i64, span: *span };
                    },
                    Type::Int32 => {
                        // provided_argument is already an int32
                    },
                    (type_) => {
                        self.push_error(SemanticError::MismatchedArgumentType {
                            expected_type: type_.clone(),
                            provided_type: Type::Int32,
                            span: *span
                        });
                    }
                }
            },

            Expression::IntLiteral64 { value, span } => {
                match &expected_argument.0 {
                    Type::Int64 => {
                        // provided_argument is already an int32
                    },
                    Type::Int32 => {
                        self.push_error(SemanticError::MismatchedArgumentType {
                            expected_type: Type::Int32,
                            provided_type: Type::Int64,
                            span: *span
                        });
                    },
                    (type_) => {
                        self.push_error(SemanticError::MismatchedArgumentType {
                            expected_type: type_.clone(),
                            provided_type: Type::Int64,
                            span: *span
                        });
                    }
                }
            },
        }

        return Ok(());
    }

    fn cast_to_default_types(&mut self, expression: &mut Expression, target_type: &Type) -> Result<Type, ()> {
        match expression {
            Expression::IntLiteral { value, span } => {
                match target_type {
                    Type::Int32 => {
                        if *value < i32::MIN as i128 || *value > i32::MAX as i128 {
                            self.push_error(SemanticError::IntegerOverflow { span: *span });
                            return Err(());
                        }

                        *expression = Expression::IntLiteral32 { value: *value as i32, span: *span };
                        return Ok(Type::Int32);
                    },
                    Type::Int64 => {
                        if *value < i64::MIN as i128 || *value > i64::MAX as i128 {
                            self.push_error(SemanticError::IntegerOverflow { span: *span });
                            return Err(());
                        }

                        *expression = Expression::IntLiteral64 { value: *value as i64, span: *span };
                        return Ok(Type::Int64);
                    },
                    _ => {
                        // Type mismatch, push an error if needed
                        self.push_error(SemanticError::InvalidTypeWidening {
                            from_type: Type::GenericInt,
                            to_type: target_type.clone(),
                            span: *span,
                        });

                        return Err(());
                    }
                }
            },

            Expression::Variable { type_, span, .. } => {
                if let Some(var_type) = type_ {
                    // Only rewrite if the variable's type is still generic
                    if var_type.same_kind(&Type::GenericInt) {
                        *type_ = Some(target_type.clone());
                        return Ok(target_type.clone());
                    }
                }

                return Err(());
            },

            Expression::BinaryOperation { left, right, span, .. } => {
                self.cast_to_default_types(left, target_type);
                self.cast_to_default_types(right, target_type);

                let left_type = self.infer_expression_type(left);
                let right_type = self.infer_expression_type(right);
                if let (Some(l), Some(r)) = (left_type, right_type) {
                    if !l.same_kind(&r) {
                        self.push_error(SemanticError::MismatchedBinaryOperationType {
                            left_type: l,
                            right_type: r,
                            span: *span,
                        });

                        return Err(());
                    }

                    return Ok(l);
                }

                return Err(());
            },
            Expression::UnaryOperation { operand, .. } => {
                return self.cast_to_default_types(operand, target_type);
            },

            Expression::FunctionCall { called, arguments, .. } => {
                return Err(());
                // Return error for now because why would a function call be generic
                
                // self.cast_to_default_types(called, target_type);

                // for arg in arguments.iter_mut() {
                //     self.cast_to_default_types(arg, target_type);
                // }
            },

            Expression::IntLiteral32 { .. } => return Ok(Type::Int32),
            Expression::IntLiteral64 { .. } => return Ok(Type::Int64),
        }
    }

    fn widen_expression(&mut self, expression: &mut Expression, target_type: &Type) {
        match expression {
            // -------- literals --------
            Expression::IntLiteral32 { value, span } => {
                if target_type.same_kind(&Type::Int64) {
                    *expression = Expression::IntLiteral64 {
                        value: *value as i64,
                        span: *span,
                    };
                }
            }

            Expression::IntLiteral64 { value, span } => {
                if target_type.same_kind(&Type::Int32) {
                    self.push_error(SemanticError::InvalidTypeWidening {
                        from_type: Type::Int64,
                        to_type: Type::Int32,
                        span: *span,
                    });
                }
            }

            Expression::Variable { .. } => {
                // Variables are already typed.
                // Widening variables would require IR-level casts,
                // not AST mutation.
            }

            Expression::BinaryOperation { left, right, span, .. } => {
                // First widen children
                self.widen_expression(left, target_type);
                self.widen_expression(right, target_type);

                // let left_type = match self.infer_expression_type(left) {
                //     Some(t) => t,
                //     None => return,
                // };

                // let right_type = match self.infer_expression_type(right) {
                //     Some(t) => t,
                //     None => return,
                // };

                // if !left_type.is_assignable_to(target_type)
                //     || !right_type.is_assignable_to(target_type)
                // {
                //     self.push_error(SemanticError::InvalidTypeWidening {
                //         from_type: left_type,
                //         to_type: target_type.clone(),
                //         span: *span,
                //     });
                // }
            }

            Expression::UnaryOperation { operand, .. } => {
                self.widen_expression(operand, target_type);
            }

            Expression::FunctionCall { .. } => {
                // Function calls already carry a return type.
                // If the call returns int32 and target is int64,
                // this widening must happen during codegen, not AST.
            }

            Expression::IntLiteral { value, span } => {
                match target_type {
                    Type::Int32 => {
                        if *value > i32::MAX as i128 || *value < i32::MIN as i128 {
                            self.push_error(SemanticError::IntegerOverflow { span: *span });
                        }
                        *expression = Expression::IntLiteral32 {
                            value: *value as i32,
                            span: *span,
                        };
                    }
                    Type::Int64 => {
                        if *value > i64::MAX as i128 || *value < i64::MIN as i128 {
                            self.push_error(SemanticError::IntegerOverflow { span: *span });
                        }
                        *expression = Expression::IntLiteral64 {
                            value: *value as i64,
                            span: *span,
                        };
                    }
                    _ => {
                        self.push_error(SemanticError::InvalidTypeWidening {
                            from_type: Type::GenericInt,
                            to_type: target_type.clone(),
                            span: *span,
                        });
                    }
                }
            }
        }
    }

    // fn resolve_binary_type(
    //     &mut self,
    //     left: Type,
    //     right: Type,
    //     span: Span,
    // ) -> Result<Type, ()> {
    //     match (left, right) {
    //         // Same concrete types
    //         (a, b) if a.same_kind(&b) => Ok(a),

    //         // Generic int + concrete int → concrete
    //         (Type::GenericInt, t) if t.is_integer() => Ok(t),
    //         (t, Type::GenericInt) if t.is_integer() => Ok(t),

    //         // Widening
    //         (Type::Int32, Type::Int64) | (Int64, Int32) => Ok(Int64),

    //         // Everything else is invalid
    //         (l, r) => {
    //             self.push_error(SemanticError::MismatchedBinaryOperationType {
    //                 left_type: l,
    //                 right_type: r,
    //                 span,
    //             });
    //             Err(())
    //         },
    //     }
    // }
}