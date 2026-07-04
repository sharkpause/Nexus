use std::mem::discriminant;

#[derive(Debug, Clone)]
pub enum Type {
    Int1, // Boolean
    Int8, // String is a Pointer(Int8)
    Int32,
    Int64,
    GenericInt, // For integer literals that will be turned into something more specific by semantic analysis
    Void,
    Null,
    Pointer(Box<Type>),

    Invalid
    // Not a real data type, a hack for the compiler to detect a non-critical error
    // has occurred and it shouldn't print the error message again to avoid duplicate
    // error messages.
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
            Type::Int32 | Type::Int64 | Type::Int8 | Type::GenericInt
        );
    }

    pub fn is_numeric(&self) -> bool {
        return matches!(
            self,
            Type::Int8 | Type::Int32 | Type::Int64 | Type::GenericInt
        );
    }

    pub fn is_bool(&self) -> bool {
        return matches!(self, Type::Int1);
    }

    pub fn is_assignable_to(&self, other: &Type) -> bool {
        return match (self, other) {
            (Type::GenericInt, t) if t.is_integer() => true,
            (t, Type::GenericInt) if t.is_integer() => true,
            (Type::Int32, Type::Int64) => true,

            (Type::Pointer(..), Type::Null) => true,
            (Type::Null, Type::Pointer(..)) => true,

            (a, b) => a.same_kind(b),
        }
    }

    pub fn is_generic(&self) -> bool {
        return match self {
            Type::GenericInt => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        return match self {
            Type::Pointer(..) => true,
            _ => false,
        }
    }

    pub fn is_invalid(&self) -> bool {
        return match self {
            Type::Invalid => true,
            _ => false
        }
    }

    // pub fn is_same_generic(&self, other: &Type) -> bool {
    //     match (self, other) {
    //         (Type::GenericInt, t) if t.is_integer() => true,
    //         (t, Type::GenericInt) if t.is_integer() => true,
    //         (Type::Int32, Type::Int64) => true,
    //         (Type::Int64, Type::Int32) => true,
    //         (a, b) => a.same_kind(b)
    //     }
    // }
}
