#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <optional>

enum class TokenType {
    _return,
    intLiteral,
    semicolon
};

struct Token {
    TokenType type;
    std::optional<std::string> value;
};

std::string readFile(const std::string& path) {
    std::ifstream input(path);
    if(!input.is_open()) {
        std::cerr << "File " << path << " does not exist\n";
        exit(EXIT_FAILURE);
    }

    return { 
        std::istreambuf_iterator<char>(input),
        std::istreambuf_iterator<char>()
    };
}

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
                tokens.push_back(Token{ TokenType::_return, std::nullopt });
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

            tokens.push_back(Token{ TokenType::intLiteral, buffer });
        } else if(currentChar == ';') {
            tokens.push_back(Token{ TokenType::semicolon, std::nullopt });
        } else if(std::isspace(currentChar)) {
            continue;
        } else {
            std::cerr << "Unexpected character: " << currentChar << "\n";
            exit(EXIT_FAILURE);
        }
    }

    return tokens;
}

int main(int argc, char* argv[]) {
    if(argc != 2) {
        std::cerr << "Incorrect usage. Correct usage:\n";
        std::cerr << "molbc {input.molb}\n";
        return EXIT_FAILURE;
    }

    std::string contents = readFile(argv[1]);
    std::cout << "content: " << contents << "\n\n";

    std::vector<Token> tokens = tokenize(contents);

    for(const auto& t : tokens) {
        switch(t.type) {
            case TokenType::_return: std::cout << "_return"; break;
            case TokenType::intLiteral: std::cout << t.value.value(); break;
            case TokenType::semicolon: std::cout << ";"; break;
        }
        std::cout << "\n";
    }

    return EXIT_SUCCESS;
}