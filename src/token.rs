use std::mem::discriminant;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Return,
    IntLiteral(i128),
    Semicolon,
    LeftBrace,
    RightBrace,
    LeftParentheses,
    RightParentheses,
    Function,
    Int32Type,
    Int64Type,
    Identifier(String),
    Plus,
    Minus,
    Star,
    Slash,
    Var,
    Equal,
    Comma,
    If,
    Else,
    While,
    DoubleEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Not,
    Break,
    Continue,
    VoidType,
    StringLiteral(String),
    StringType,
    Extern
}

impl Token {
    // pub fn same_variant(&self, other: &Token) -> bool {
    //     return discriminant(&self.kind) == discriminant(&other.kind);
    // }

    pub fn same_kind(&self, other: &TokenKind) -> bool {
        return discriminant(&self.kind) == discriminant(&other);
    }
}

pub fn print_token(token: &Token) {
    println!("{:?}", token);
}
