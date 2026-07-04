use crate::{
    errors::SemanticError,
    parsing::{expression::Expression, span::Span, types::Type},
    semantics::{semantic_context::SemanticContext, symbols::VariableSymbol},
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

    pub fn validate(&mut self, context: &mut SemanticContext) {
        match self {
            Statement::Block { statements, span } => {
                context.enter_scope();

                for statement in statements {
                    statement.validate(context);
                }

                context.exit_scope();
            }

            Statement::VariableInitialize {
                var_type,
                name,
                initializer,
                span,
            } => {
                let mut error_happened = false;
                if context.lookup_variable_in_current_scope(name).is_some() {
                    context.push_error(SemanticError::DuplicateVariable {
                        name: name.clone(),
                        span: *span,
                    });
                    error_happened = true;
                }
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
                    context.push_error(SemanticError::MismatchedAssignmentType {
                        expected_type: var_type.clone(),
                        provided_type: inferred_type.clone(),
                        span: *span,
                    });
                }

                initializer.coerce_to(var_type.clone());
                context.insert_variable(
                    name.clone(),
                    VariableSymbol {
                        type_: var_type.clone(),
                        span: *span,
                    },
                );
            }

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
            }

            _ => {
                todo!("a");
            }
        }
    }
}
