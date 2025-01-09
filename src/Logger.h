#ifndef LOGGER_H
#define LOGGER_H

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>

class ErrorLogMessage : public std::basic_ostringstream<char> {
public:
    ~ErrorLogMessage() {
        std::cerr << "Fatal error: " << str().c_str() << std::endl;
        exit(EXIT_FAILURE);
    }
};

#define DIE ErrorLogMessage()
#endif// LOGGER_H