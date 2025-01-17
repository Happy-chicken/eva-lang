#ifndef EVALLVM_H
#define EVALLVM_H
#include <cstddef>
#include <llvm-14/llvm/BinaryFormat/Dwarf.h>
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Constants.h>
#include <llvm-14/llvm/IR/Instructions.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Casting.h>
#include <map>
#include <memory>
#include <regex>
#include <string>
#include <vector>

#include "Environment.h"
#include "EvaParser.h"
#include "Logger.h"

// env type
using Env = std::shared_ptr<Environment>;

// Generic binary operator:
#define GEN_BINARY_OP(Op, varName)             \
    do {                                       \
        auto op1 = gen(exp.list[1], env);      \
        auto op2 = gen(exp.list[2], env);      \
        return builder->Op(op1, op2, varName); \
    } while (false)

// class info
struct ClassInfo {
    llvm::StructType *cls;
    llvm::StructType *parent;
    std::map<std::string, llvm::Type *> fieldsMap;
    std::map<std::string, llvm::Function *> methodsMap;
};

static const size_t RESERVED_FILED_COUNT = 1;
static const size_t VTABLE_INDEX = 0;

class EvaLLVM {

public:
    EvaLLVM() : parser(std::make_unique<syntax::EvaParser>()) {
        moduleInit();
        setupExternFunctions();
        setupGlobalEnvironment();
        setupTargetTriple();
    }
    ~EvaLLVM() = default;
    void exec(const std::string &program);// run the program

private:
    /*compile ast*/
    void compile(const Exp &ast);

    /*Main compile loop*/
    llvm::Value *gen(const Exp &exp, Env env);

    void moduleInit();
    void saveModuleToFile(const std::string &fileName);//save the file to the given path
    void setupGlobalEnvironment();                     //setup the global environment
    void setupTargetTriple();                          // setup target triple
    void setupExternFunctions();                       //define external functions from libc++

    size_t getMethodIndex(llvm::StructType *cls, const std::string &methodName) {
        auto methods = &classMap_[cls->getName().data()].methodsMap;
        auto it = methods->find(methodName);
        return std::distance(methods->begin(), it);
    }

    size_t getFieldIndex(llvm::StructType *cls, const std::string &fieldName) {
        auto fields = &classMap_[cls->getName().data()].fieldsMap;
        auto it = fields->find(fieldName);
        return std::distance(fields->begin(), it) + RESERVED_FILED_COUNT;
    }

    /*create global variable*/
    llvm::GlobalVariable *createGlobalVar(const std::string &name, llvm::Constant *init);

    llvm::Value *allocVar(const std::string &name, llvm::Type *type, Env env);

    /*create a function*/
    llvm::Function *createFunction(const std::string &fnName, llvm::FunctionType *fnType, Env env);

    /*create a function prototype*/
    llvm::Function *createFunctionProto(const std::string &fnName, llvm::FunctionType *fnType, Env env);

    /* create a basic block */
    llvm::BasicBlock *createBB(const std::string &name, llvm::Function *fn = nullptr);

    /* create a function block */
    void createFunctionBlock(llvm::Function *fn);

    // (def square ((x number)) -> number (* x x))
    llvm::Value *compileFunction(const Exp &fnExp, std::string fnName, Env env);

    // create instance
    llvm::Value *createInstance(const Exp &exp, Env env, const std::string &varName);

    // allocate an object of a given class on the heap
    llvm::Value *mallocInstance(llvm::StructType *cls, const std::string &name);

    // return size of type in bytes
    size_t getTypeSize(llvm::Type *type) {
        return module->getDataLayout().getTypeAllocSize(type);
    }

    // inherit parent class field
    void inheritClass(llvm::StructType *cls, llvm::StructType *parent);

    // build class info
    void buildClassInfo(llvm::StructType *cls, const Exp &clsExp, Env env);

    // build class body
    void buildClassBody(llvm::StructType *cls);

    void buildVTable(llvm::StructType *cls, ClassInfo *classInfo);

    bool isTaggedList(const Exp &exp, const std::string &tag) {
        return exp.type == ExpType::LIST && exp.list[0].type == ExpType::SYMBOL && exp.list[0].string == tag;
    }

    bool isVar(const Exp &exp) {
        return isTaggedList(exp, "var");
    }

    bool isDef(const Exp &exp) {
        return isTaggedList(exp, "def");
    }

    bool isNew(const Exp &exp) {
        return isTaggedList(exp, "new");
    }

    bool isProp(const Exp &exp) {
        return isTaggedList(exp, "prop");
    }

    bool isSuper(const Exp &exp) {
        return isTaggedList(exp, "super");
    }

    /*get type from string*/
    llvm::Type *getTypeFromString(const std::string &type) {
        if (type == "number") {
            return builder->getInt32Ty();
        } else if (type == "string") {
            return builder->getInt8PtrTy()->getPointerTo();
        }
        // class
        return classMap_[type].cls->getPointerTo();
        // return builder->getInt32Ty();
    }

    llvm::Type *excrateVarType(Exp &expr) {
        return expr.type == ExpType::LIST ? getTypeFromString(expr.list[1].string) : builder->getInt32Ty();
    }

    std::string excrateVarName(Exp &expr) {
        return expr.type == ExpType::LIST ? expr.list[0].string : expr.string;
    }

    bool hasReturnType(const Exp &fnExp) {
        return fnExp.list[3].type == ExpType::SYMBOL && fnExp.list[3].string == "->";
    }

    llvm::StructType *getClassByName(const std::string &name) {
        return llvm::StructType::getTypeByName(*ctx, name);
    }

    llvm::FunctionType *excrateFunType(const Exp &fnExp) {
        auto params = fnExp.list[2];
        auto returnType = hasReturnType(fnExp) ? getTypeFromString(fnExp.list[4].string) : builder->getInt32Ty();
        std::vector<llvm::Type *> paramTypes{};

        for (auto &param: params.list) {
            auto paramType = excrateVarType(param);
            auto paramName = excrateVarName(param);
            paramTypes.push_back(paramName == "self" ? (llvm::Type *) cls->getPointerTo() : paramType);
        }

        return llvm::FunctionType::get(returnType, paramTypes, false);
    }

private:
    Env globalEnv;                                 // global environment (symbol table)
    std::unique_ptr<syntax::EvaParser> parser;     // define the parser
    llvm::Function *fn;                            // current compiling function
    std::unique_ptr<llvm::LLVMContext> ctx;        // container for modules and other LLVM objects
    std::unique_ptr<llvm::Module> module;          // container for functions and global variables
    std::unique_ptr<llvm::IRBuilder<>> builder;    // helps to generate IR
    std::unique_ptr<llvm::IRBuilder<>> varsBuilder;// helps to generate IR
    llvm::StructType *cls = nullptr;               // current compiling class type
    std::map<std::string, ClassInfo> classMap_;    // class map
};

#endif