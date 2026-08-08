use crate::{errors::SemanticError, parsing::types::Type, semantics::semantic_context::SemanticContext};

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,          // +
    Subtract,     //    -
    Multiply,     //    *
    Divide,       // /
    Equal,        // ==
    NotEqual,     // !=
    LessThan,     // <
    GreaterThan,  // >
    LessEqual,    // <=
    GreaterEqual, // >=
    And,          // &&
    Or,           // ||
    Not,          //    !
    Modulo,       // %,
    BitAnd,       //    &
    BitOr,        // |
    BitXor,       // ^
    ShiftLeft,    // <<
    ShiftRight,   // >>
}

impl Operator {
    pub fn validate(&self, left: Option<&Type>, right: &Type) -> bool {
        return match self {
            Operator::Not => left.is_none() && right.same_kind(&Type::Int1),
             Operator::Subtract => {
                match left {
                    Some(left_type) => left_type.is_numeric() && right.is_numeric(),

                    None => right.is_numeric(),
                }
            },

            Operator::Add | Operator::Multiply | Operator::Divide | Operator::Modulo => {
                match left {
                    Some(left_type) => left_type.is_numeric() && right.is_numeric(),
                
                    None => false
                }
            }

            Operator::LessEqual | Operator::LessThan
            | Operator::GreaterEqual | Operator::GreaterThan => {
                match left {
                    Some(left_type) => left_type.is_numeric() && right.is_numeric(),

                    None => false
                }
            }

            Operator::Equal | Operator::NotEqual => {
                match left {
                    Some(left_type) => {
                        left_type.is_assignable_to(right)
                        || right.is_assignable_to(left_type)
                    },

                    None => false,
                }
            },

            Operator::And | Operator::Or => {
                match left {
                    Some(left_type) => left_type.is_bool() && right.is_bool(),

                    None => false
                }
            },

            Operator::BitAnd | Operator::BitOr
            | Operator::BitXor | Operator::ShiftLeft
            | Operator::ShiftRight => {
                match left {
                    Some(left_type) => left_type.is_integer() && right.is_integer(),

                    None => false
                }
            }

            // _ => {
            //     todo!()
            // }
        };
    }

    pub fn unary_result_type(&self, operand_type: &Type) -> Type {
        if operand_type.is_invalid() {
            return Type::Invalid;
        }

        return match self {
            Operator::Subtract => {
                operand_type.clone()
            },

            Operator::Not => {
                Type::Int1
            },

            Operator::BitAnd | Operator::Multiply => {
                return todo!("Pointers are not implemented yet")
            },

            _ => {
                Type::Invalid
            }
        }
    }
}
