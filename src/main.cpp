#include "lexer.h"
#include "readFile.h"
#include "generateCode.h"

#include <cstdlib>
#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
    if(argc != 2) {
        std::cerr << "Incorrect usage. Correct usage:\n";
        std::cerr << "molbc {input.molb}\n";
        return EXIT_FAILURE;
    }

    std::string contents = readFile(argv[1]);
    std::cout << "content: " << contents << "\n\n";

    std::vector<Token> tokens = tokenize(contents);

    // for(const auto& t : tokens) {
    //     switch(t.type) {
    //         case TokenType::_return: std::cout << "_return"; break;
    //         case TokenType::intLiteral: std::cout << t.value.value(); break;
    //         case TokenType::semicolon: std::cout << ";"; break;
    //     }
    //     std::cout << "\n";
    // }
    
    {
        std::fstream outputFile("./out.asm", std::ios::out);
        outputFile << generateCode(tokens);
    }

    system("nasm -f elf64 out.asm");
    system("ld out.o");

    return EXIT_SUCCESS;
}