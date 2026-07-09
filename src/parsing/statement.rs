use crate::{
    errors::SemanticError, parsing::{expression::{Expression, ExpressionKind}, span::Span, types::Type}, semantics::{semantic_context::SemanticContext, symbols::VariableSymbol},
};

#[derive(Debug, Clone)]
pub enum Statement {
    Return {
        value: Option<Expression>,
        span: Span,
    },

    VariableInitialize {
        var_type: Type,
        name: String,
        initializer: Expression,
        span: Span,
    },

    VariableAssignment {
        name: String,
        value: Expression,
        span: Span,
    },

    Block {
        statements: Vec<Statement>,
        span: Span,
    },

    Expression {
        expression: Expression,
        span: Span,
    },

    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
        span: Span,
    },

    While {
        condition: Expression,
        body: Box<Statement>,
        span: Span,
    },

    Break {
        span: Span,
    },

    Continue {
        span: Span,
    },
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Return { span, .. } => *span,
            Statement::VariableInitialize { span, .. } => *span,
            Statement::VariableAssignment { span, .. } => *span,
            Statement::Block { span, .. } => *span,
            Statement::Expression { span, .. } => *span,
            Statement::If { span, .. } => *span,
            Statement::While { span, .. } => *span,
            Statement::Break { span } => *span,
            Statement::Continue { span } => *span,
        }
    }

    pub fn validate(&mut self, context: &mut SemanticContext, create_scope: bool) {
        match self {
            Statement::Block { statements, span, } => {
                if create_scope {
                    context.enter_scope();
                }

                for statement in statements {
                    statement.validate(context, create_scope);
                }

                if create_scope {
                    context.exit_scope();
                }
            },

            Statement::VariableInitialize {
                var_type,
                name,
                initializer,
                span,
            } => {
                let mut error_happened = false;
                if var_type.same_kind(&Type::Void) {
                    context.push_error(SemanticError::InvalidType {
                        var_name: name.clone(),
                        var_type: var_type.clone(),
                        span: *span,
                    });
                    error_happened = true;
                }

                if error_happened == true {
                    return;
                }

                initializer.validate(context);

                let inferred_type = initializer.infer_type(context);
                if inferred_type.is_invalid() {
                    return;
                }

                if !inferred_type.is_assignable_to(var_type) {
                    return context.push_error(SemanticError::MismatchedAssignmentType {
                        expected_type: var_type.clone(),
                        provided_type: inferred_type.clone(),
                        span: *span,
                    });
                }

                initializer.coerce_to(var_type.clone());
                if context.insert_variable(
                    name.clone(),
                    VariableSymbol {
                        type_: var_type.clone(),
                        span: *span,
                    }).is_err() {
                    context.push_error(SemanticError::DuplicateVariable {
                        name: name.clone(),
                        span: self.span()
                    });
                }
            },

            Statement::VariableAssignment {
                name,
                value,
                span } => {
                value.validate(context);
                let inferred_type = value.infer_type(context);
                if inferred_type.is_invalid() {
                    return;
                }
                
                let Some(variable) = context.lookup_variable(name) else {
                    return context.push_error(SemanticError::UndefinedVariable {
                        name: name.to_string(),
                        span: *span
                    });
                };

                if !inferred_type.is_assignable_to(&variable.type_) {
                    return context.push_error(SemanticError::MismatchedAssignmentType {
                        expected_type: variable.type_.clone(),
                        provided_type: inferred_type,
                        span: *span,
                    });
                }

                value.coerce_to(variable.type_.clone());
            },

            Statement::Expression { expression, span } => {
                expression.validate(context);
                if expression.infer_type(context).is_invalid() {
                    return;
                }

                 match expression.kind {
                    ExpressionKind::FunctionCall { .. } => {}

                    _ => {
                        context.push_error(
                            SemanticError::UselessExpression {
                                span: *span,
                            }
                        );
                    }
                }
            },

            Statement::Return { value, span } => {
                if let Some(return_expression) = value {
                    return_expression.validate(context);

                    let inferred_type = return_expression.infer_type(context);
                    if inferred_type.is_invalid() {
                        return;
                    }

                    if !inferred_type.is_assignable_to(&context.current_return_type) {
                        return context.push_error(SemanticError::MismatchedReturnType {
                            expected_return_type: context.current_return_type.clone(),
                            provided_return_type: inferred_type,
                            span: *span,
                        });
                    }

                    return return_expression.coerce_to(context.current_return_type.clone());
                }

                if !context.current_return_type.same_kind(&Type::Void) {
                    return context.push_error(SemanticError::MismatchedReturnType {
                        expected_return_type: context.current_return_type.clone(),
                        provided_return_type: Type::Void,
                        span: *span,
                    });
                }
            },

            Statement::If {
                condition,
                then_branch,
                else_branch,
                span
            } => {
                condition.validate(context);
                let condition_type = condition.infer_type(context);
                if condition_type.is_invalid() {
                    return;
                }

                if !condition_type.is_bool() {
                    context.push_error(SemanticError::InvalidConditionType {
                        provided_type: condition_type,
                        span: *span
                    });
                }

                then_branch.validate(context, create_scope);
                if let Some(else_body) = else_branch {
                    else_body.validate(context, create_scope);
                }
            },

            Statement::While {
                condition,
                body,
                span
            } => {
                condition.validate(context);
                let condition_type = condition.infer_type(context);
                if condition_type.is_invalid() {
                    return;
                }

                if !condition_type.is_bool() {
                    context.push_error(SemanticError::InvalidConditionType {
                        provided_type: condition_type,
                        span: *span
                    });
                }

                context.enter_loop();
                body.validate(context, create_scope);
                context.exit_loop();
            },

            Statement::Break { span } => {
                if context.loop_depth == 0 {
                    context.push_error(SemanticError::BreakOutsideLoop { span:*span });
                }
            },

            Statement::Continue { span } => {
                if context.loop_depth == 0 {
                    context.push_error(SemanticError::ContinueOutsideLoop { span: *span });
                }
            }
        }
    }
}
