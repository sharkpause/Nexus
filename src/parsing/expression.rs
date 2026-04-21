use crate::parsing::types::Type;
use crate::parsing::span::Span;
use crate::parsing::operator::Operator;
use crate::semantics::semantic_context::SemanticContext;

#[derive(Debug, Clone)]
pub enum Expression {
    Variable {
        name: String,
        type_: Option<Type>,
        span: Span,
    },

    IntLiteral {
        value: i128,
        span: Span,
    },

    IntLiteral8 {
        value: i8,
        span: Span,
    },
    
    IntLiteral32 {
        value: i32,
        span: Span
    },

    IntLiteral64 {
        value: i64,
        span: Span
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
        span: Span
    },

    BooleanLiteral {
        value: bool,
        span: Span
    },

    NullLiteral {
        span: Span
    },
}

impl Expression {
    // pub fn same_kind(&self, other: &Expression) -> bool {
    //     return discriminant(self) == discriminant(other);
    // }

    // pub fn is_null(&self) -> bool {
    //     return matches!(self, Expression::Null { .. })
    // }

    pub fn infer_type(&mut self, context: &mut SemanticContext) {

    }
}