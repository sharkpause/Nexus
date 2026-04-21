use crate::parsing::{expression::Expression, span::Span, types::Type};

#[derive(Debug, Clone)]
pub enum Statement {
    Return {
        value: Option<Expression>,
        span: Span,
    },

    VariableDeclare {
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
            Statement::VariableDeclare { span, .. } => *span,
            Statement::VariableAssignment { span, .. } => *span,
            Statement::Block { span, .. } => *span,
            Statement::Expression { span, .. } => *span,
            Statement::If { span, .. } => *span,
            Statement::While { span, .. } => *span,
            Statement::Break { span } => *span,
            Statement::Continue { span } => *span,
        }
    }
}
