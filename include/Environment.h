#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "./Logger.h"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>
#include <map>
#include <memory>
#include <string>

class Environment : public std::enable_shared_from_this<Environment> {
public:
    Environment(std::map<std::string, llvm::Value *> record, std::shared_ptr<Environment> parent)
        : parent_(std::move(parent)), record_(std::move(record)) {}
    ~Environment() = default;

    // create variable with given name and value
    llvm::Value *define(const std::string &name, llvm::Value *value) {
        record_[name] = value;
        return value;
    }

    // return the value of the defiend variable, otherwise throw an exception
    llvm::Value *lookup(const std::string &name) {
        return resolve(name)->record_[name];
    }

private:
    // return specific environment where the variable is defined, otherwise throw an exception
    std::shared_ptr<Environment> resolve(const std::string &name) {
        if (record_.count(name) != 0) {// current environment has the variable
            return shared_from_this();
        }
        if (parent_ == nullptr) {// no parent environment
            DIE << "Variable: " << name << " is not defined";
        }
        return parent_->resolve(name);
    }
    // binding storage
    std::map<std::string, llvm::Value *> record_;

    // parent environment link
    std::shared_ptr<Environment> parent_;
};

#endif// ENVIRONMENT_H