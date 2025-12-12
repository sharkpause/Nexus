#include "lexer.h"

#include <cctype>
#include <cstdio>
#include <iostream>

std::vector<Token> tokenize(const std::string& str) {
    std::vector<Token> tokens;
    std::string buffer;

    for(size_t i = 0; i < str.size(); i++) {
        char currentChar = str[i];

        if(std::isalpha(currentChar)) {
            buffer.clear();
            buffer.push_back(currentChar);
            i++;

            while(i < str.size() && std::isalnum(str[i])) {
                buffer.push_back(str[i]);
                i++;
            }
            i--;

            if(buffer == "return") {
                tokens.push_back(Token{
                     TokenType::_return, 
                     std::nullopt 
                }
            );
            } else {
                std::cerr << "Unexpected identifier: " << buffer << "\n";
                exit(EXIT_FAILURE);
            }
        }  else if(std::isdigit(currentChar)) {
            buffer.clear();
            buffer.push_back(currentChar);
            i++;

            while(i < str.size() && std::isdigit(str[i])) {
                buffer.push_back(str[i]);
                i++;
            }
            i--;

            tokens.push_back(Token{
                 TokenType::intLiteral,
                 buffer
                }
            );
        } else if(currentChar == ';') {
            tokens.push_back(Token{
                 TokenType::semicolon,
                 std::nullopt
                }
            );
        } else if(std::isspace(currentChar)) {
            continue;
        } else {
            std::cerr << "Unexpected character: " << currentChar << "\n";
            exit(EXIT_FAILURE);
        }
    }

    return tokens;
}