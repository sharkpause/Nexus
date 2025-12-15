#include "readFile.hpp"

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