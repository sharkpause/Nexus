#pragma once

#include <vector>
#include <string>
#include <optional>

enum class TokenType {
    exit,
    intLiteral,
    semicolon
};

struct Token {
    TokenType type;
    std::optional<std::string> value;
};

class Tokenizer {
    public:
        inline explicit Tokenizer(std::string source)
            : mSource(std::move(source))
        {

        }

        std::vector<Token> tokenize();
    private:
        [[nodiscard]] std::optional<char> peek(const int ahead = 1) const {
            if(mIndex + ahead > mSource.length()) {
                return {};
            }

            return mSource.at(mIndex);
        }

        char consume() {
            return mSource.at(mIndex++);
        }

        std::string mSource;
        size_t mIndex = 0;
};