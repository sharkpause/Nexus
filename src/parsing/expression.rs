use std::mem::discriminant;

use crate::errors::SemanticError;
use crate::parsing::operator::Operator;
use crate::parsing::span::Span;
use crate::parsing::types::Type::{self, Invalid};
use crate::semantics::semantic_context::SemanticContext;

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub type_: Option<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Variable {
        name: String,
        type_: Option<Type>,
    },

    IntLiteral {
        value: i128,
    },

    BinaryOperation {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },

    UnaryOperation {
        operator: Operator,
        operand: Box<Expression>,
    },

    FunctionCall {
        called: Box<Expression>,
        arguments: Vec<Expression>,
    },

    StringLiteral {
        value: String,
    },

    BooleanLiteral {
        value: bool,
    },

    NullLiteral
}

impl Expression {
    pub fn same_kind(&self, other: &Expression) -> bool {
        return discriminant(&self.kind) == discriminant(&other.kind);
    }

    pub fn is_null(&self) -> bool {
        return matches!(self.kind, ExpressionKind::NullLiteral { .. });
    }

    pub fn infer_type(&mut self, context: &mut SemanticContext) -> Type {
        if let Some(existing) = &self.type_ {
            return existing.clone();
        }

        let inferred_type = match &mut self.kind {
            ExpressionKind::NullLiteral { .. } => Type::Null,

            ExpressionKind::BooleanLiteral { .. } => Type::Int1,

            ExpressionKind::IntLiteral { .. } => Type::GenericInt,

            ExpressionKind::StringLiteral { .. } => Type::Pointer(Box::new(Type::Int8)),

            ExpressionKind::Variable { name, .. } => context
                .lookup_variable_type(name.as_str())
                .unwrap_or(Type::Invalid),

            ExpressionKind::BinaryOperation {
                left,
                operator,
                right,
            } => {
                let left_type = left.infer_type(context);
                let right_type = right.infer_type(context);

                context.unify_binary_types(&left_type, &right_type, operator)
            }

            ExpressionKind::UnaryOperation {
                operator,
                operand,
            } => {
                let operand_type = operand.infer_type(context);
                operator.unary_result_type(&operand_type)
            }

            ExpressionKind::FunctionCall {
                called,
                ..
            } => called.infer_type(context),
        };

        self.type_ = Some(inferred_type.clone());

        return inferred_type;
    }

    pub fn coerce_to(&mut self, target_type: Type) {
        self.type_ = Some(target_type.clone());

        match &mut self.kind {
            ExpressionKind::BinaryOperation { left, right, .. } => {
                left.coerce_to(target_type.clone());
                right.coerce_to(target_type.clone());
            }

            _ => {}
        }
    }

    pub fn validate(&mut self, context: &mut SemanticContext) {
        match &mut self.kind {
            ExpressionKind::Variable { name, .. } => {
                if context.lookup_variable(name).is_none() {
                    context.push_error(crate::errors::SemanticError::UndefinedVariable {
                        name: name.clone(),
                        span: self.span,
                    });

                    self.type_ = Some(Type::Invalid);
                };
            }

            ExpressionKind::BinaryOperation {
                left,
                operator,
                right,
            } => {
                left.validate(context);
                right.validate(context);

                let left_type = left.infer_type(context);
                let right_type = right.infer_type(context);

                if left_type.is_invalid() || right_type.is_invalid() {
                    self.type_ = Some(Type::Invalid);
                    return;
                }

                if !operator.validate(Some(&left_type), &right_type) {
                    context.push_error(SemanticError::MismatchedBinaryOperationType {
                        left_type,
                        right_type,
                        span: self.span,
                    });

                    self.type_ = Some(Type::Invalid);
                }
            }

            ExpressionKind::UnaryOperation {
                operator,
                operand,
            } => {
                operand.validate(context);
                let operand_type = operand.infer_type(context);

                if operand_type.is_invalid() {
                    self.type_ = Some(Type::Invalid);
                    return;
                }

                if !operator.validate(None, &operand_type) {
                    context.push_error(SemanticError::InvalidUnaryOperation {
                        operand_type,
                        span: self.span,
                    });
                    self.type_ = Some(Type::Invalid);
                }
            }

            ExpressionKind::FunctionCall {
                called,
                arguments
            } => {
                // TODO:
                // Currently Nexus only supports direct function calls:
                //     foo()
                //
                // Function pointers can be supported later by allowing
                // `called` to be any expression whose type resolves to:
                //
                //     Pointer(Function {
                //         parameters: Vec<Type>,
                //         return_type: Type,
                //     })
                //
                // This would enable C-style callbacks:
                //     var callback = &foo;
                //     callback();
                //
                // For now, only lookup named functions from function_table.

                match &called.kind {
                    ExpressionKind::Variable { name, type_ } => {
                        let Some(function) = context.lookup_function(name) else {
                            context.push_error(SemanticError::UndefinedVariable {
                                name: name.clone(),
                                span: self.span,
                            });

                            self.type_ = Some(Type::Invalid);
                            return;
                        };

                        self.type_ = Some(function.return_type.clone());

                        if arguments.len() != function.parameters.len() {
                            return context.push_error(SemanticError::MismatchedArgumentCount {
                                called_function_name: name.clone(),
                                provided_argument_count: arguments.len(),
                                expected_argument_count: function.parameters.len(),
                                span: self.span,
                            });
                        }

                        for (argument, parameter) in
                            arguments.iter_mut().zip(function.parameters.clone())
                        {
                            argument.validate(context);

                            let argument_type = argument.infer_type(context);

                            if argument_type.is_invalid() {
                                self.type_ = Some(Type::Invalid);
                                continue;
                            }

                            if !argument_type.is_assignable_to(&parameter.0) {
                                context.push_error(SemanticError::MismatchedArgumentType {
                                    expected_type: parameter.0,
                                    provided_type: argument_type,
                                    span: self.span,
                                });

                                continue;
                            }

                            argument.coerce_to(parameter.0);
                        }
                    }

                    _ => unreachable!(
                        "called in function call expressions should always be a variable"
                    ),
                };
            }

            ExpressionKind::IntLiteral { value } => {
                // accepts
            }

            ExpressionKind::BooleanLiteral { value } => {
                // accepts
            }

            ExpressionKind::StringLiteral { value } => {
                // accepts
            }

            ExpressionKind::NullLiteral { .. } => {
                // accepts
            }
        }
    }
}
