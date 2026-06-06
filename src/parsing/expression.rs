use std::mem::discriminant;

use crate::parsing::operator::Operator;
use crate::parsing::span::Span;
use crate::parsing::types::Type;
use crate::semantics::semantic_context::SemanticContext;

// #[derive(Debug, Clone)]
// pub enum Expression {
//     Variable {
//         name: String,
//         type_: Option<Type>,
//         span: Span,
//     },

//     IntLiteral {
//         value: i128,
//         span: Span,
//     },

//     IntLiteral8 {
//         value: i8,
//         span: Span,
//     },

//     IntLiteral32 {
//         value: i32,
//         span: Span
//     },

//     IntLiteral64 {
//         value: i64,
//         span: Span
//     },

//     BinaryOperation {
//         left: Box<Expression>,
//         operator: Operator,
//         right: Box<Expression>,
//         span: Span,
//     },

//     UnaryOperation {
//         operator: Operator,
//         operand: Box<Expression>,
//         span: Span,
//     },

//     FunctionCall {
//         called: Box<Expression>,
//         arguments: Vec<Expression>,
//         span: Span,
//     },

//     StringLiteral {
//         value: String,
//         span: Span
//     },

//     BooleanLiteral {
//         value: bool,
//         span: Span
//     },

//     NullLiteral {
//         span: Span
//     },
// }

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub type_: Option<Type>, // TODO: Move span from ExpressionKind to here
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Variable {
        name: String,
        type_: Option<Type>,
        span: Span,
    },

    IntLiteral {
        value: i128,
        span: Span,
    },

    BinaryOperation {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
        span: Span,
    },

    UnaryOperation {
        operator: Operator,
        operand: Box<Expression>,
        span: Span,
    },

    FunctionCall {
        called: Box<Expression>,
        arguments: Vec<Expression>,
        span: Span,
    },

    StringLiteral {
        value: String,
        span: Span,
    },

    BooleanLiteral {
        value: bool,
        span: Span,
    },

    NullLiteral {
        span: Span,
    },
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

            ExpressionKind::Variable { name, .. } => {
                context.lookup_variable_type(name.as_str()).unwrap()
            }

            ExpressionKind::BinaryOperation {
                left,
                operator,
                right,
                span,
            } => {
                let left_type = left.infer_type(context);
                let right_type = right.infer_type(context);

                context.unify_binary_types(&left_type, &right_type, operator)
            }

            _ => {
                unimplemented!("Later");
            }
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
            ExpressionKind::Variable { name, span, .. } => {
                if context.lookup_variable(name).is_none() {
                    context.push_error(crate::errors::SemanticError::UndefinedVariable {
                        name: name.clone(),
                        span: *span,
                    });
                };
            }

            ExpressionKind::BinaryOperation {
                left,
                operator,
                right,
                span
            } => {
                left.validate(context);
                right.validate(context);

                let left_type = left.infer_type(context);
                let right_type = right.infer_type(context);

                if !operator.validate(Some(&left_type), &right_type) {
                    context.push_error(
                        crate::errors::SemanticError::MismatchedBinaryOperationType {
                            left_type,
                            right_type,
                            span: *span,
                        },
                    );
                }
            }

            _ => {}
        }
    }
}
