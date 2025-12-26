mod token;
mod lexer;
mod parser;
mod codegen;

use std::{env, fs, process::Command};

use crate::token::print_token;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::codegen::{ CodeGenerator };

fn read_file(path: &String) -> String {
    let source_code =
        fs::read_to_string(path).expect("Failed to read source code");

    return source_code;
}

fn write_file(path: String, contents: &String) {
    fs::write(path, contents).expect("Failed to write assembly");
}

fn assemble_and_link(asm_path: &str, output_exe: &str) {
    let nasm_status = Command::new("nasm")
        .args(&["-f", "elf64", asm_path, "-o", "out.o"])
        .status()
        .expect("Failed to run NASM");

    if !nasm_status.success() {
        panic!("Assembling failed");
    }

    let ld_status = Command::new("ld")
        .args(&["out.o", "-o", output_exe])
        .status()
        .expect("Failed to run LD");

    if !ld_status.success() {
        panic!("Linking failed");
    }

    println!("Executable '{}' produced", output_exe);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Incorrect usage");
        return;
    }

    let input = read_file(&args[1]);

    let mut lexer = Lexer {
        input,
        index: 0,
    };

    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("lexer error: {:?}", e);
            return;
        }
    };

    println!("Tokens:");
    for token in &tokens {
        print_token(token);
    }

    let mut parser = Parser {
        tokens,
        index: 0,
    };

    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(e) => {
            eprintln!("parser error: {:?}", e);
            return;
        }
    };

    println!("\nAST:\n{:#?}", program);

    let mut codegen = CodeGenerator::default();

    let output = match codegen.generate(program) {
        Ok(output) => output,
        Err(e) => {
            eprintln!("codegen error: {:?}", e);
            return;
        }
    };

    println!("{}", output);

    write_file(String::from("out.asm"), &output);
    assemble_and_link("out.asm", "out");
}