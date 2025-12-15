#pragma once

#include "lexer.hpp"

#include <sstream>

std::string generateCode(const std::vector<Token>& tokens);