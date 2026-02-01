use crate::{lexer::LexerError, token::{ Token, TokenKind }};
use std::mem::discriminant;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub enum ParserError {
    EndOfInput,
    GenericError,
    UnexpectedToken(Token),
    UnexpectedEndOfInput,
    UnexpectedType(Type),
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Function(Function),
    Statement(Statement),
}

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
}

impl Expression {
    // pub fn same_kind(&self, other: &Expression) -> bool {
    //     return discriminant(self) == discriminant(other);
    // }

    // pub fn is_null(&self) -> bool {
    //     return matches!(self, Expression::Null { .. })
    // }
}

#[derive(Debug, Clone)]
pub enum Type {
    Int32,
    Int64,
    GenericInt, // For integer literals
    Void,
    String
}

impl Type {
    pub fn same_kind(&self, other: &Type) -> bool {
        return discriminant(self) == discriminant(other);
    }

    pub fn is_void(&self) -> bool {
        return discriminant(self) == discriminant(&Type::Void);
    }

    pub fn is_integer(&self) -> bool {
        return matches!(
            self,
            Type::Int32 | Type::Int64 | Type::GenericInt
        );
    }

    pub fn is_assignable_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::GenericInt, t) if t.is_integer() => true,
            (t, Type::GenericInt) if t.is_integer() => true,
            (Type::Int32, Type::Int64) => true,
            (a, b) => a.same_kind(b)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,           // +
    Subtract,      // -
    Multiply,      // *
    Divide,        // /
    Equal,         // ==
    NotEqual,      // !=
    LessThan,      // <
    GreaterThan,   // >
    LessEqual,     // <=
    GreaterEqual,  // >=
    And,           // &&
    Or,            // ||
    Not,           // !
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<(Type, String)>,
    pub body: Statement,
    pub span: Span,
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub index: usize,
}

impl Parser {
    pub fn from(tokens: Vec<Token>) -> Self {
        // Potentially change to take ownership of tokens instead of copy for performance and memory usage
        return Self {
            tokens,
            index: 0
        };
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        return self.tokens.get(self.index + offset);
    }

    fn consume_token(&mut self) {
        self.index += 1;
    }

    pub fn expect_token(&mut self, expected: &TokenKind) -> Result<Token, ParserError> {
        let token = self.peek_token(0).ok_or(ParserError::UnexpectedEndOfInput)?.clone();
        
         if token.same_kind(expected) {
            self.consume_token();
            return Ok(token);
        } else {
            return Err(ParserError::UnexpectedToken(token.clone()));
        }
    }

    pub fn expect_type(&mut self) -> Result<Type, ParserError> {
        let token = self
            .peek_token(0)
            .ok_or(ParserError::UnexpectedEndOfInput)?;

        match &token.kind {
            TokenKind::Int32Type => {
                self.consume_token();
                return Ok(Type::Int32);
            },
            
            TokenKind::Int64Type => {
                self.consume_token();
                return Ok(Type::Int64);
            },
            
            TokenKind::VoidType => {
                self.consume_token();
                return Ok(Type::Void);
            },

            TokenKind::StringType => {
                self.consume_token();
                return Ok(Type::String);
            }
            
            _ => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    pub fn expect_identifier(&mut self) -> Result<String, ParserError> {
        let token = self.peek_token(0).ok_or(ParserError::UnexpectedEndOfInput)?;

        if let TokenKind::Identifier(value) = &token.kind {
            let name = value.clone();
            self.consume_token();
            Ok(name)
        } else {
            Err(ParserError::UnexpectedToken(token.clone()))
        }
    }

    fn binding_power(&self, token: &Token) -> Option<u8> {
        /*
        highest
        -------
        ! (not)
        * / 
        + -
        < <= > >=
        == !=
        && 
        ||
        -------
        lowest
        */

        return match &token.kind {
            TokenKind::Or => Some(1),
            TokenKind::And => Some(2),
            TokenKind::DoubleEqual | TokenKind::NotEqual => Some(3),
            TokenKind::LessThan | TokenKind::LessEqual => Some(4),
            TokenKind::GreaterThan | TokenKind::GreaterEqual => Some(5),
            TokenKind::Plus | TokenKind::Minus => Some(6),
            TokenKind::Star | TokenKind::Slash => Some(7),
            TokenKind::Not => Some(8),
            _ => None
        };
    }

    fn token_to_operator(&self, token: &Token) -> Option<Operator> {
        return match &token.kind {
            TokenKind::Plus => Some(Operator::Add),
            TokenKind::Minus => Some(Operator::Subtract),
            TokenKind::Star => Some(Operator::Multiply),
            TokenKind::Slash => Some(Operator::Divide),
            TokenKind::DoubleEqual => Some(Operator::Equal),
            TokenKind::NotEqual => Some(Operator::NotEqual),
            TokenKind::LessThan => Some(Operator::LessThan),
            TokenKind::LessEqual => Some(Operator::LessEqual),
            TokenKind::GreaterThan => Some(Operator::GreaterThan),
            TokenKind::GreaterEqual => Some(Operator::GreaterEqual),
            TokenKind::Not => Some(Operator::Not),
            TokenKind::And => Some(Operator::And),
            TokenKind::Or => Some(Operator::Or),
            _ => None
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<TopLevel>, ParserError> {
        let mut program: Vec<TopLevel> = Vec::new();

        while self.index < self.tokens.len() {
            let token = self.peek_token(0).ok_or(ParserError::UnexpectedEndOfInput)?;
            match &token.kind {
                TokenKind::Function => {
                    let function = self.parse_function()?;
                    program.push(TopLevel::Function(function));
                },
                _ => {
                    let statement = self.parse_statement()?;
                    program.push(TopLevel::Statement(statement));
                }
            }
        }

        Ok(program)
    }

    pub fn parse_function(&mut self) -> Result<Function, ParserError> {
        let start_token = self.expect_token(&TokenKind::Function)?;
        let span = Span {
            line: start_token.line,
            column: start_token.column
        };

        let return_type = self.expect_type()?;

        let function_name = self.expect_identifier()?;

        self.expect_token(&TokenKind::LeftParentheses)?;

        let mut parameters: Vec<(Type, String)> = Vec::new();
        while let Some(token) = self.peek_token(0) {
            if token.same_kind(&TokenKind::RightParentheses) {
                break;
            }
        
            let parameter_type = self.expect_type()?;
            let parameter_name = self.expect_identifier()?;
            parameters.push((parameter_type, parameter_name));
        
            if let Some(token) = self.peek_token(0) {
                if token.same_kind(&TokenKind::Comma) {
                    self.consume_token();
                }
            }
        }

        self.consume_token();

        return Ok(Function {
            name: function_name,
            return_type: return_type,
            parameters,
            body: self.parse_block()?,
            span
        });
    }

    pub fn parse_block(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.expect_token(&TokenKind::LeftBrace)?;
        let statements = self.parse_statements()?;

        self.expect_token(&TokenKind::RightBrace)?;

        let span = Span {
            line: start_token.line,
            column: start_token.column,
        };

        return Ok(Statement::Block { statements, span });
    }

    pub fn parse_statements(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements = Vec::new();

        while let Some(token) = self.peek_token(0) {
            if token.same_kind(&TokenKind::RightBrace) {
                break;
            }
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        return Ok(statements);
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.peek_token(0)
            .ok_or(ParserError::UnexpectedEndOfInput)?;
        let span = Span {
            line: token.line,
            column: token.column
        };

        match &token.kind {
            TokenKind::LeftBrace => self.parse_block(),

            TokenKind::Return => {
                self.consume_token();

                match self.expect_token(&TokenKind::Semicolon) {
                    Ok(_) => {
                        return Ok(Statement::Return { value: None, span });
                    },
                    Err(_) => {
                        let expression = self.parse_expression(0)?;
                        self.expect_token(&TokenKind::Semicolon);

                        return Ok(Statement::Return { value: Some(expression), span });   
                    }
                }
            }

            TokenKind::Var => {
                self.consume_token();
                
                let variable_type = self.expect_type()?;
                let variable_name = self.expect_identifier()?;
                
                self.expect_token(&TokenKind::Equal)?;
                let initializer = self.parse_expression(0)?;
                
                self.expect_token(&TokenKind::Semicolon)?;
                
                return Ok(Statement::VariableDeclare {
                    var_type: variable_type,
                    name: variable_name,
                    initializer,
                    span
                });
            }

            TokenKind::If => {
                self.consume_token();
                
                let mut conditions = vec![self.parse_expression(0)?];
                let mut bodies = vec![self.parse_block()?];

                // else if
                while let Some(current_token) = self.peek_token(0) {
                    if current_token.same_kind(&TokenKind::Else) {
                        if let Some(next_token) = self.peek_token(1) {
                            if next_token.same_kind(&TokenKind::If) {
                                self.consume_token(); // else
                                self.consume_token(); // if
                                conditions.push(self.parse_expression(0)?);
                                bodies.push(self.parse_block()?);
                                continue;
                            }
                        }
                        break;
                    } else {
                        break;
                    }
                }

                let else_body = if let Some(tok) = self.peek_token(0) {
                    if tok.same_kind(&TokenKind::Else) {
                        self.consume_token();
                        Some(Box::new(self.parse_block()?))
                    } else {
                        None
                    }
                } else {
                    None
                };

                // parse nested ifs, doing it recursively to avoid stack overflow with deep recursion
                let mut result = else_body;
                for (cond, body) in conditions.iter().rev().zip(bodies.iter().rev()) {
                    result = Some(Box::new(Statement::If {
                        condition: cond.clone(),
                        then_branch: Box::new(body.clone()),
                        else_branch: result,
                        span
                    }));
                }

                return Ok(*result.expect("Expected at least one if/else branch"));
            }

            TokenKind::While => {
                self.consume_token();
                
                self.expect_token(&TokenKind::LeftParentheses)?;
                let condition = self.parse_expression(0)?;
                
                self.expect_token(&TokenKind::RightParentheses)?;
                let body = Box::new(self.parse_statement()?);
                
                return Ok(Statement::While { condition, body, span });
            }

            TokenKind::Break => {
                self.consume_token();
                self.expect_token(&TokenKind::Semicolon)?;
                 
                return Ok(Statement::Break { span });
            }

            TokenKind::Continue => {
                self.consume_token();
                self.expect_token(&TokenKind::Semicolon)?;
                
                return Ok(Statement::Continue{ span });
            }

            TokenKind::Identifier(_) => {
                let name = if let TokenKind::Identifier(n) = &token.kind {
                    n.clone()
                } else { unreachable!() };

                // Evaluate an assignment
                if let Some(next_token) = self.peek_token(1) {
                    if next_token.same_kind(&TokenKind::Equal) {
                        self.consume_token(); // identifier
                        self.consume_token(); // =

                        let expression = self.parse_expression(0)?;
                        
                        self.expect_token(&TokenKind::Semicolon)?;
                        
                        return Ok(Statement::VariableAssignment { name, value: expression, span });
                    }
                }

                // fallback to expression
                let expression = self.parse_expression(0)?;
                self.expect_token(&TokenKind::Semicolon)?;
                
                // An expression evaluated for its side effects. Yeah this looks weird
                // Currently used primarily for function calls where the return value is discarded.
                return Ok(Statement::Expression { expression, span });
            }

            _ => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    pub fn parse_expression(&mut self, min_bp: u8) -> Result<Expression, ParserError> {
        let current_token = self
            .peek_token(0)
            .ok_or(ParserError::UnexpectedEndOfInput)?;

        let span = Span {
            line: current_token.line,
            column: current_token.column
        };

        let mut lhs = match &current_token.kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.consume_token();
                    
                if !self.peek_token(0).map_or(false, |token| token.same_kind(&TokenKind::LeftParentheses)) {
                    Expression::Variable { name, span, type_: None }
                } else {
                    self.consume_token(); // consume '('
                    let mut arguments = Vec::new();
                    
                    while !self.peek_token(0).map_or(false, |token| token.same_kind(&TokenKind::RightParentheses)) {
                        arguments.push(self.parse_expression(0)?);
                    
                        if self.peek_token(0).map_or(false, |token| token.same_kind(&TokenKind::Comma)) {
                            self.consume_token();
                        } else {
                            break;
                        }
                    }
                
                    self.expect_token(&TokenKind::RightParentheses)?;
                
                    Expression::FunctionCall {
                        called: Box::new(Expression::Variable { name, span, type_: None }),
                        arguments,
                        span
                    }
                }
            },

            TokenKind::IntLiteral(number) => {
                let value = *number;
                self.consume_token();
                
                Expression::IntLiteral { value, span }
            },

            TokenKind::LeftParentheses => {
                self.consume_token();
                let parsed_expression = self.parse_expression(0)?;
                self.expect_token(&TokenKind::RightParentheses)?;
                
                parsed_expression
            },

            TokenKind::Minus => {
                self.consume_token();
                let expression = self.parse_expression(6)?;
                
                Expression::UnaryOperation {
                    operator: Operator::Subtract,
                    operand: Box::new(expression),
                    span
                }
            },

            TokenKind::Not => {
                self.consume_token();
                let expression = self.parse_expression(8)?;
                
                Expression::UnaryOperation {
                    operator: Operator::Not,
                    operand: Box::new(expression),
                    span
                }
            },

            TokenKind::StringLiteral(value) => {
                let expression_value = value.clone();

                self.consume_token();
                
                Expression::StringLiteral {
                    value: expression_value,
                    span
                }
            }

            _ => return Err(ParserError::UnexpectedToken(current_token.clone())),
        };

        loop {
            let operator_token = self
                .peek_token(0)
                .cloned()
                .ok_or(ParserError::UnexpectedEndOfInput)?;

            let bp = self.binding_power(&operator_token);

            match bp {
                Some(bp) if bp >= min_bp => {
                    self.consume_token();
                    let rhs_min_bp = bp + 1;
                    let rhs = self.parse_expression(rhs_min_bp)?;

                    let operator = self
                        .token_to_operator(&operator_token)
                        .ok_or(ParserError::UnexpectedToken(operator_token))?;

                    lhs = Expression::BinaryOperation {
                        left: Box::new(lhs),
                        operator,
                        right: Box::new(rhs),
                        span
                    };
                }
                _ => break,
            }
        }

        return Ok(lhs);
    }

}