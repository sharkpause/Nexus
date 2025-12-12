#pragma once

#include "lexer.h"

#include <sstream>

std::string generateCode(const std::vector<Token>& tokens);