#include "generateCode.hpp"
#include <iostream>
std::string generateCode(const std::vector<Token>& tokens) {
    std::stringstream output;

    output << "global _start\n_start:\n";

    for(int i = 0; i < tokens.size(); i++) {
        const Token& token = tokens.at(i);

        if(token.type == TokenType::exit) {
            if(i + 1 < tokens.size() && tokens.at(i + 1).type == TokenType::intLiteral) {
                if(i + 2 < tokens.size() && tokens.at(i + 2).type == TokenType::semicolon) {
                    output << "    mov rax, 60\n";
                    output << "    mov rdi, " << tokens.at(i+1).value.value() << "\n";
                    output << "    syscall";
                }
            }
        }
    }

    return output.str();
}