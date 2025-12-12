#pragma once

#include <vector>
#include <string>
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

std::vector<Token> tokenize(const std::string& str);