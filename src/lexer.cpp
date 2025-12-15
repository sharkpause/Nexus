#include "lexer.hpp"

#include <cctype>
#include <cstdlib>
#include <iostream>

std::vector<Token> Tokenizer::tokenize() {
    std::vector<Token> tokens;
    std::string buffer;

    while(peek().has_value()) {
        if(std::isalpha(peek().value())) {
            buffer.push_back(consume());
            
            while(peek().has_value() && std::isalnum(peek().value())) {
                buffer.push_back(consume());
            }

            if(buffer == "exit") {
                tokens.push_back({
                    .type = TokenType::exit
                });

                buffer.clear();
                continue;
            } else {
                std::cerr << "Unexpected identifier: " << buffer << std::endl;
                exit(EXIT_FAILURE);
            }
        } else if(std::isdigit(peek().value())) {
            buffer.push_back(consume());

            while(peek().has_value() && std::isdigit(peek().value())) {
                buffer.push_back(consume());
            }

            tokens.push_back({
                .type = TokenType::intLiteral,
                .value = buffer
            });

            buffer.clear();
            continue;
        } else if(peek().value() == ';') {
            tokens.push_back({
                .type = TokenType::semicolon
            });

            consume();

            continue;
        } else if(std::isspace(peek().value())) {
            consume();
            continue;
        } else {
            std::cerr << "You fucked up" << std::endl;
            exit(EXIT_FAILURE);
        }
    }

    mIndex = 0;

    return tokens;
}