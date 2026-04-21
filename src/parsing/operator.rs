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
    Modulo,        // %,
    BitAnd,        // &
    BitOr,         // |
    BitXor,        // ^
    ShiftLeft,     // <<
    ShiftRight,    // >>
}