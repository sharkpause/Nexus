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
        matches!(
            self,
            Type::Int8 | Type::Int32 | Type::Int64 | Type::GenericInt
        )
    }

    pub fn is_assignable_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::GenericInt, t) if t.is_integer() => true,
            (t, Type::GenericInt) if t.is_integer() => true,
            (Type::Int32, Type::Int64) => true,

            (Type::Pointer(..), Type::Null) => true,
            (Type::Null, Type::Pointer(..)) => true,

            (a, b) => a.same_kind(b),
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Type::GenericInt => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(..) => true,
            _ => false,
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
