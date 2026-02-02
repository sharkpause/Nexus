# Nexus Programming Language

Nexus is a C-like programming language with nicer memory management.  
It achieves this by utilizing runtime features like leak detection, wrapped allocator functions to make double frees safe, prevent use after frees, prevent the creation of dangling pointers, and more.

##### Current progress: smol, the language is still very bare-bones and does not have many basic features and primitive types yet. Note that the nicer memory management features has not been implemented yet

---

## Overview

- **Type System:** `int32`, `int64`, `string`, `void`
- **Code Generation:** LLVM IR, NASM (deprecated).
- **Goals:** Fast, safe, minimal overhead, easy to extend.

---

## Architecture

- **Lexer:** Transforms source code that is raw text into tokens.
- **Parser:** Builds an abstract syntax tree to represent the program.
- **Semantic Analyzer:** Performs type checking and adoptions, ensures variable/function existence, enforces scope rules, etc.
- **Codegen:** Converts AST to the targeted backend, handles variables, functions, control flow, string etc.
- **LLVM IR Compilation:** Optimized with `opt` and `llc`, linked to produce executable with `clang`

---

## Building & Running

### Compile Nexus code to executable
```bash
# Example Nexus source file: hello.nex
git clone https://github.com/sharkpause/nexus.git
cd nexus
cargo build --release
cd target
./nexus hello.nex

