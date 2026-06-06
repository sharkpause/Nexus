use crate::parsing::types::Type;

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,          // +
    Subtract,     // -
    Multiply,     // *
    Divide,       // /
    Equal,        // ==
    NotEqual,     // !=
    LessThan,     // <
    GreaterThan,  // >
    LessEqual,    // <=
    GreaterEqual, // >=
    And,          // &&
    Or,           // ||
    Not,          // !
    Modulo,       // %,
    BitAnd,       // &
    BitOr,        // |
    BitXor,       // ^
    ShiftLeft,    // <<
    ShiftRight,   // >>
}

impl Operator {
    pub fn validate(&self, left: Option<&Type>, right: &Type) -> bool {
        match self {
            Operator::Not => left.is_none() && right.same_kind(&Type::Int1),

            Operator::Add | Operator::Subtract | Operator::Multiply | Operator::Divide => {
                match left {
                    Some(left_type) => left_type.is_numeric() && right.is_numeric(),

                    None => right.is_numeric(),
                }
            }

            _ => {
                todo!()
            }
        }
    }
}
