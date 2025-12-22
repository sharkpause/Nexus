use crate::parser::{Expression, Statement};

pub fn codegen(statements: Vec<Statement>) -> String {
    let mut output = String::new();

    for statement in statements {
        match statement {
            Statement::Return(Expression::IntLiteral(value)) => {
                output.push_str(&format!("    mov rax, {}\n", value));
                output.push_str("    ret\n");
            }
        }
    }

    return output;
}