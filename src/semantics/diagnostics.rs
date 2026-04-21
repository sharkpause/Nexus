use crate::errors::SemanticError;

#[derive(Debug)]
pub struct Diagnostics {
    pub errors: Vec<SemanticError>
}

impl Diagnostics {
    pub fn has_fatal(&self) -> bool {
        return self.errors.iter().any(|error| error.is_fatal());
    }

    pub fn has_errors(&self) -> bool {
        return self.errors.len() > 0;
    }
}
