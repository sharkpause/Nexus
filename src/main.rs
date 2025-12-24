mod token;
mod lexer;
mod parser;
mod codegen;

use std::{env, fs, process::Command};

use crate::token::print_token;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::codegen::{CodeGenerator, CodegenError};

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

    let mut lexer = Lexer {
        input: read_file(&args[1]),
        index: 0
    };

    let tokens = lexer.tokenize().unwrap();

    println!("Tokens:");

    for token in &tokens {
        print_token(token);
    }

    let mut parser = Parser {
        tokens: tokens,
        index: 0
    };

    let program = parser.parse_program().unwrap();
    println!("\nStatements:\n{:?}", program);

    let mut codegenerator = CodeGenerator::default();

    let output = codegenerator.generate(program).unwrap();
    write_file(String::from("out.asm"), &output);
    assemble_and_link("out.asm", "out");
}