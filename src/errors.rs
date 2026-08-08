use crate::{lexing::token::Token, parsing::{span::Span, types::Type}};



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
    MissingReturn {
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
    InvalidConditionType {
        provided_type: Type,
        span: Span
    },
    InvalidUnaryOperation {
        operand_type: Type,
        span: Span
    },
    UselessExpression {
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
        return matches!(
            self,
            SemanticError::NoEntryFunction
            | SemanticError::InvalidTopLevelStatement { .. }
        );
    }
}

#[derive(Debug)]
pub enum ParserError {
    EndOfInput,
    GenericError,
    UnexpectedToken(Token),
    UnexpectedEndOfInput,
    UnexpectedType(Type),
    UnexpectedBody,
}