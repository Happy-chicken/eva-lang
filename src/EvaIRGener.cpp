#include "../include/EvaIRGener.h"

void EvaLLVM::exec(const std::string &program)// run the program
{
    // 1. parse the program
    auto ast = parser->parse("(begin " + program + ")");
    // 2. generate the LLVM IR
    compile(ast);
    // print the generated IR
    module->print(llvm::outs(), nullptr);
    // 3. save module to file
    saveModuleToFile("./output.ll");
}

void EvaLLVM::compile(const Exp &ast) {
    // create a main function
    fn = createFunction("main", llvm::FunctionType::get(builder->getInt32Ty(), false), globalEnv);

    // create global variable
    createGlobalVar("VERSION", builder->getInt32(1));

    // compile body of the main function, generate IR
    gen(ast, globalEnv);

    // return the result 0
    builder->CreateRet(builder->getInt32(0));
}

/*Main compile loop*/
llvm::Value *EvaLLVM::gen(const Exp &exp, Env env) {
    switch (exp.type) {
        case ExpType::NUMBER:// numbers
            return builder->getInt32(exp.number);
        case ExpType::STRING:// strings
        {
            auto re = std::regex("\\\\n");
            auto str = std::regex_replace(exp.string, re, "\n");
            return builder->CreateGlobalString(str);
        }

        case ExpType::SYMBOL:// symbols (variables, operators)
        {
            // boolean
            if (exp.string == "true" || exp.string == "false") {
                return builder->getInt1(exp.string == "true" ? true : false);
            } else {
                // variable
                auto varName = exp.string;
                auto value = env->lookup(varName);

                // local variable
                if (auto localVar = llvm::dyn_cast<llvm::AllocaInst>(value)) {
                    return builder->CreateLoad(localVar->getAllocatedType(), localVar, varName.c_str());
                }

                // global variable
                else if (auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(value)) {
                    return builder->CreateLoad(globalVar->getInitializer()->getType(), globalVar, varName.c_str());
                }

                // treat as a funciton
                else {
                    return value;
                }
            }
        }
        case ExpType::LIST:// lists
        {
            auto tag = exp.list[0];
            // special case
            if (tag.type == ExpType::SYMBOL) {
                auto op = tag.string;
                // control flow if
                if (op == "if") {
                    auto conditon = gen(exp.list[1], env);

                    auto thenBlock = createBB("then", fn);
                    // else, if-end block to handle nested if expression
                    auto elseBlock = createBB("else");
                    auto ifEndBlock = createBB("ifend");

                    // conditon branch
                    builder->CreateCondBr(conditon, thenBlock, elseBlock);
                    // then branch
                    builder->SetInsertPoint(thenBlock);
                    auto thenRes = gen(exp.list[2], env);
                    builder->CreateBr(ifEndBlock);
                    // restore block to handle nested if expression
                    // which is needed for phi instruction
                    thenBlock = builder->GetInsertBlock();
                    // else branch
                    // append block to function
                    elseBlock->insertInto(fn);// in llvm 17
                    // fn->getBasicBlockList().push_back(elseBlock); in llvm 14
                    builder->SetInsertPoint(elseBlock);
                    auto elseRes = gen(exp.list[3], env);
                    builder->CreateBr(ifEndBlock);
                    // same for else block
                    elseBlock = builder->GetInsertBlock();
                    // endif
                    ifEndBlock->insertInto(fn);
                    // fn->getBasicBlockList().push_back(ifEndBlock);
                    builder->SetInsertPoint(ifEndBlock);

                    // result of if expression is phi
                    auto phi = builder->CreatePHI(thenRes->getType(), 2, "tmpif");
                    phi->addIncoming(thenRes, thenBlock);
                    phi->addIncoming(elseRes, elseBlock);
                    return phi;
                } else if (op == "while") {
                    // condition
                    auto condBlock = createBB("cond", fn);
                    builder->CreateBr(condBlock);

                    // Body:while end loop
                    auto bodyBlock = createBB("body");
                    auto loopEndBlock = createBB("loopend");

                    // compile while
                    builder->SetInsertPoint(condBlock);
                    auto condition = gen(exp.list[1], env);

                    // condition branch
                    builder->CreateCondBr(condition, bodyBlock, loopEndBlock);

                    // body
                    bodyBlock->insertInto(fn);
                    builder->SetInsertPoint(bodyBlock);
                    gen(exp.list[2], env);

                    // jump to condition block unconditionally
                    builder->CreateBr(condBlock);

                    // end while
                    loopEndBlock->insertInto(fn);
                    builder->SetInsertPoint(loopEndBlock);
                    return builder->getInt32(0);
                }
                // arithmetic operation
                if (op == "+") {
                    GEN_BINARY_OP(CreateAdd, "tmpadd");
                } else if (op == "-") {
                    GEN_BINARY_OP(CreateSub, "tmpsub");
                } else if (op == "*") {
                    GEN_BINARY_OP(CreateMul, "tmpmul");
                } else if (op == "/") {
                    GEN_BINARY_OP(CreateSDiv, "tmpdiv");
                }// Unsigned comparison
                else if (op == ">") {
                    GEN_BINARY_OP(CreateICmpUGT, "tmpcmp");
                } else if (op == "<") {
                    GEN_BINARY_OP(CreateICmpULT, "tmpcmp");
                } else if (op == "==") {
                    GEN_BINARY_OP(CreateICmpEQ, "tmpcmp");
                } else if (op == "!=") {
                    GEN_BINARY_OP(CreateICmpNE, "tmpcmp");
                } else if (op == ">=") {
                    GEN_BINARY_OP(CreateICmpUGE, "tmpcmp");
                } else if (op == "<=") {
                    GEN_BINARY_OP(CreateICmpULE, "tmpcmp");
                }
                // Function: def <name> <params> <body>
                if (op == "def") {
                    return compileFunction(exp, exp.list[1].string, env);
                }
                // variable decalration: (var x (+ y 10))
                // (var (x number) 10)
                if (op == "var") {

                    // special case for class fields,
                    // which are already defined in class info allocation
                    if (cls != nullptr) {
                        return builder->getInt32(0);
                    }
                    auto varDecl = exp.list[1];
                    auto varName = excrateVarName(varDecl);

                    if (isNew(exp.list[2])) {
                        auto instance = createInstance(exp.list[2], env, varName);
                        return env->define(varName, instance);
                    }

                    auto init = gen(exp.list[2], env);
                    auto varTy = excrateVarType(varDecl);
                    auto varBinding = allocVar(varName, varTy, env);
                    return builder->CreateStore(init, varBinding);
                }
                // variable update
                else if (op == "set") {

                    auto value = gen(exp.list[2], env);
                    // (set (prop self x) x)
                    if (isProp(exp.list[1])) {
                        auto instance = gen(exp.list[1].list[1], env);
                        auto fieldName = exp.list[2].string;
                        auto ptrName = std::string("p") + fieldName;
                        auto cls = (llvm::StructType *) (instance->getType()->getContainedType(0));
                        // field index
                        auto fieldIdx = getFieldIndex(cls, fieldName);

                        auto address = builder->CreateStructGEP(cls, instance, fieldIdx, ptrName);
                        builder->CreateStore(value, address);
                        return value;
                    } else {
                        auto varName = exp.list[1].string;
                        // variable
                        auto varBinding = env->lookup(varName);

                        builder->CreateStore(value, varBinding);
                        return value;
                    }

                }
                // Block: (begin <expression>)
                else if (op == "begin") {
                    // compile each expression in the block
                    // result is the last expression
                    auto blockEnv = std::make_shared<Environment>(std::map<std::string, llvm::Value *>{}, env);
                    llvm::Value *blockRes = nullptr;
                    for (auto i = 1; i < exp.list.size(); i++) {
                        blockRes = gen(exp.list[i], blockEnv);
                    }
                    return blockRes;
                }

                // extern function call
                else if (op == "print") {
                    // call printf
                    auto printfFn = module->getFunction("printf");
                    // arguments
                    std::vector<llvm::Value *> args = {};
                    for (auto i = 1; i < exp.list.size(); i++) {
                        args.push_back(gen(exp.list[i], env));
                    }
                    return builder->CreateCall(printfFn, args);
                }
                // (class <name> <super> <body>)
                else if (op == "class") {
                    auto name = exp.list[1].string;
                    auto parent = exp.list[2].string == "null" ? nullptr : getClassByName(exp.list[2].string);

                    // now compile the class
                    cls = llvm::StructType::create(*ctx, name);
                    // ! need to delete
                    module->getOrInsertGlobal(name, cls);
                    // super class data always sit at the beginning
                    if (parent != nullptr) {
                        inheritClass(cls, parent);
                    } else {
                        // add class info
                        classMap_[name] = {cls, parent, /*field*/ {}, /*method*/ {}};
                    }
                    // populate the class with fields and methods
                    buildClassInfo(cls, exp, env);

                    // compile the class body
                    gen(exp.list[3], env);

                    // reset class after compilation, so normal function does not pick the class name prefix
                    cls = nullptr;

                    return builder->getInt32(0);
                }
                // instance
                else if (op == "new") {
                    return createInstance(exp, env, "");
                }
                // prop access: (prop <instance> <name>)
                else if (op == "prop") {
                    // instance
                    auto instance = gen(exp.list[1], env);
                    auto fieldName = exp.list[2].string;

                    auto ptrName = std::string("p") + fieldName;
                    auto cls = (llvm::StructType *) (instance->getType()->getContainedType(0));
                    // field index
                    auto fieldIdx = getFieldIndex(cls, fieldName);

                    auto address = builder->CreateStructGEP(cls, instance, fieldIdx, ptrName);
                    return builder->CreateLoad(cls->getElementType(fieldIdx), address, fieldName);
                }
                // (method <instance> <name>)
                // (method (super <class>) <name>)
                else if (op == "method") {
                    auto methodName = exp.list[2].string;

                    llvm::StructType *cls = nullptr;
                    llvm::Value *vTable = nullptr;
                    llvm::StructType *vTableType = nullptr;

                    if (isSuper(exp.list[1])) {
                        auto className = exp.list[1].list[1].string;
                        cls = classMap_[className].parent;
                        auto parentName = std::string(cls->getName().data());

                        vTable = module->getNamedGlobal(parentName + "_vTable");
                        vTableType = llvm::StructType::getTypeByName(*ctx, parentName + "_vTable");
                    } else {
                        // just instance
                        auto instance = gen(exp.list[1], env);
                        cls = (llvm::StructType *) (instance->getType()->getContainedType(0));

                        // load vtable
                        auto vTableAddr = builder->CreateStructGEP(cls, instance, VTABLE_INDEX);
                        vTable = builder->CreateLoad(cls->getElementType(VTABLE_INDEX), vTableAddr, "vtable");
                        vTableType = (llvm::StructType *) (vTable->getType()->getContainedType(0));
                    }
                    // load methods from vtable
                    auto methodIdx = getMethodIndex(cls, methodName);
                    auto methodType = (llvm::FunctionType *) vTableType->getElementType(methodIdx);
                    auto methodAddr = builder->CreateStructGEP(vTableType, vTable, methodIdx);
                    return builder->CreateLoad(methodType, methodAddr);

                } else {
                    // fuction call
                    auto callee = gen(exp.list[0], env);

                    std::vector<llvm::Value *> args{};
                    auto argIndex = 0;

                    auto calleeType = callee->getType()->getContainedType(0);
                    if (calleeType->isStructTy()) {
                        // if it is a class
                        auto cls = (llvm::StructType *) calleeType;
                        std::string className{cls->getName().data()};

                        // push self as the first argument
                        args.push_back(callee);
                        argIndex++;

                        // TODO support polymorphism (inheritance)
                        callee = module->getFunction(className + "___call__");// 3 _
                    }
                    auto fn = (llvm::Function *) callee;

                    for (int i = 1; i < exp.list.size(); i++, argIndex++) {
                        auto argValue = gen(exp.list[i], env);
                        auto paramType = fn->getArg(argIndex)->getType();
                        auto bitCastArgVal = builder->CreateBitCast(argValue, paramType);
                        args.push_back(bitCastArgVal);
                    }

                    return builder->CreateCall(fn, args);
                }
            }
            // not symbol
            // method calls
            // ((method p getX) 2)
            else {
                auto loadedMethod = (llvm::LoadInst *) gen(exp.list[0], env);

                auto fnType = (llvm::FunctionType *) (loadedMethod->getPointerOperand()
                                                          ->getType()
                                                          ->getContainedType(0)
                                                          ->getContainedType(0));

                std::vector<llvm::Value *> args{};
                for (int i = 1; i < exp.list.size(); i++) {
                    auto argValue = gen(exp.list[i], env);
                    // Need to cast parameter type to support polymorphism(sub class)
                    // we should be able to pass Point3D instance for the type
                    // of the parent class Point
                    auto paramType = fnType->getParamType(i - 1);
                    if (argValue->getType() != paramType) {
                        auto bitCastArgVal = builder->CreateBitCast(argValue, paramType);
                        args.push_back(bitCastArgVal);
                    } else {
                        args.push_back(argValue);
                    }
                }
                return builder->CreateCall(fnType, loadedMethod, args);
            }
        }
        default:// unreachable
            return builder->getInt32(0);
    }
}

/*save the file to the given path*/
void EvaLLVM::saveModuleToFile(const std::string &fileName) {
    std::error_code errorCode;
    llvm::raw_fd_ostream outLL(fileName, errorCode);
    module->print(outLL, nullptr);
}
/*initialize the module and builder*/
void EvaLLVM::moduleInit() {
    ctx = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("eva", *ctx);

    // create a new builder for the module
    builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
    varsBuilder = std::make_unique<llvm::IRBuilder<>>(*ctx);
}

/*define external functions from libc++*/
void EvaLLVM::setupExternFunctions() {
    // i8* to substitute for char*, void* and etc.
    auto bytePtrTy = builder->getInt8PtrTy()->getPointerTo();

    // printf
    module->getOrInsertFunction("printf", llvm::FunctionType::get(builder->getInt32Ty(), bytePtrTy, true));

    // void* malloc(size_t size)
    module->getOrInsertFunction("GC_malloc", llvm::FunctionType::get(bytePtrTy, builder->getInt64Ty(), false));
}

/*setup the global environment*/
void EvaLLVM::setupGlobalEnvironment() {
    std::map<std::string, llvm::Value *> globalObjects = {
        {"VERSION", builder->getInt32(42)},
    };
    std::map<std::string, llvm::Value *> globalRecords = {};

    for (auto &obj: globalObjects) {
        globalRecords[obj.first] = createGlobalVar(obj.first, (llvm::Constant *) obj.second);
    }

    globalEnv = std::make_shared<Environment>(globalRecords, nullptr);
}

// setup target triple
void EvaLLVM::setupTargetTriple() {
    module->setTargetTriple("x86_64-unknown-linux-gnu");
}

/*create global variable*/
llvm::GlobalVariable *EvaLLVM::createGlobalVar(const std::string &name, llvm::Constant *init) {
    module->getOrInsertGlobal(name, init->getType());
    auto var = module->getNamedGlobal(name);
    var->setAlignment(llvm::MaybeAlign(4));
    var->setConstant(false);
    var->setInitializer(init);
    return var;
}

llvm::Value *EvaLLVM::allocVar(const std::string &name, llvm::Type *type, Env env) {
    varsBuilder->SetInsertPoint(&fn->getEntryBlock());
    auto varAlloc = varsBuilder->CreateAlloca(type, 0, name.c_str());

    env->define(name, varAlloc);
    return varAlloc;
}

/*create a function*/
llvm::Function *EvaLLVM::createFunction(const std::string &fnName, llvm::FunctionType *fnType, Env env) {
    // function prototype might already exist in the module
    auto fn = module->getFunction(fnName);
    if (fn == nullptr) {
        fn = createFunctionProto(fnName, fnType, env);
    }

    // create basic block
    createFunctionBlock(fn);

    return fn;
}

/*create a function prototype*/
llvm::Function *EvaLLVM::createFunctionProto(const std::string &fnName, llvm::FunctionType *fnType, Env env) {
    auto fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, fnName, module.get());
    verifyFunction(*fn);

    // install in environment
    env->define(fnName, fn);
    return fn;
}

/* create a basic block */
llvm::BasicBlock *EvaLLVM::createBB(const std::string &name, llvm::Function *fn) {
    return llvm::BasicBlock::Create(*ctx, name, fn);
}

/* create a function block */
void EvaLLVM::createFunctionBlock(llvm::Function *fn) {
    auto entry = createBB("entry", fn);
    builder->SetInsertPoint(entry);
}

// (def square ((x number)) -> number (* x x))
llvm::Value *EvaLLVM::compileFunction(const Exp &fnExp, std::string fnName, Env env) {
    auto params = fnExp.list[2];
    auto funBody = hasReturnType(fnExp) ? fnExp.list[5] : fnExp.list[3];

    // save current function
    auto prevFn = fn;
    auto prevBlock = builder->GetInsertBlock();

    auto orignalName = fnName;
    // class method
    if (cls != nullptr) {
        fnName = std::string(cls->getName().data()) + "_" + fnName;
    }

    // override fn to compile body
    auto newFn = createFunction(fnName, excrateFunType(fnExp), env);
    fn = newFn;

    // set parameter name
    auto idx = 0;
    auto fnEnv = std::make_shared<Environment>(std::map<std::string, llvm::Value *>{}, env);

    for (auto &arg: fn->args()) {
        auto param = params.list[idx++];
        auto argName = excrateVarName(param);

        arg.setName(argName);

        // allocate a local variable prr argument to make sure arguments mutable
        auto argBinding = allocVar(argName, arg.getType(), fnEnv);
        builder->CreateStore(&arg, argBinding);
    }

    builder->CreateRet(gen(funBody, fnEnv));

    // restore previous env after compiling
    builder->SetInsertPoint(prevBlock);
    fn = prevFn;

    return newFn;
}

// create instance
llvm::Value *EvaLLVM::createInstance(const Exp &exp, Env env, const std::string &varName) {
    auto className = exp.list[1].string;
    auto cls = getClassByName(className);

    if (cls == nullptr) {
        DIE << "[EvaVM]: Undefined class: " << cls;
    }
    // now it is stack allocation, TODO heap allocation
    // auto instance = varName.empty() ? builder->CreateAlloca(cls) : builder->CreateAlloca(cls, 0, varName);

    // we dont use stack allocation for obj,
    //since we need to spport constructor pattern
    // i.e. return an obj from a callee to caller, outside
    auto instance = mallocInstance(cls, varName);

    // call constructor
    auto ctor = module->getFunction(className + "_constructor");

    std::vector<llvm::Value *> args{instance};

    for (auto i = 2; i < exp.list.size(); i++) {
        args.push_back(gen(exp.list[i], env));
    }

    builder->CreateCall(ctor, args);
    return instance;
}

// allocate an object of a given class on the heap
llvm::Value *EvaLLVM::mallocInstance(llvm::StructType *cls, const std::string &name) {
    auto typeSize = builder->getInt64(getTypeSize(cls));

    auto mallocPtr = builder->CreateCall(module->getFunction("GC_malloc"), typeSize, name);

    // void* -> Point*
    auto instance = builder->CreatePointerCast(mallocPtr, cls->getPointerTo());

    // set vtable
    std::string className{cls->getName().data()};
    auto vTableName = className + "_vTable";
    auto vTableAddr = builder->CreateStructGEP(cls, instance, VTABLE_INDEX);
    auto vTable = module->getNamedGlobal(vTableName);

    builder->CreateStore(vTable, vTableAddr);

    return instance;
}

// inherit parent class field
void EvaLLVM::inheritClass(llvm::StructType *cls, llvm::StructType *parent) {
    auto parentClassInfo = &classMap_[parent->getName().data()];

    // inherit the field and method
    classMap_[cls->getName().data()] = {cls, parent, parentClassInfo->fieldsMap, parentClassInfo->methodsMap};
}

// build class info
void EvaLLVM::buildClassInfo(llvm::StructType *cls, const Exp &clsExp, Env env) {
    auto className = clsExp.list[1].string;
    auto classInfo = &classMap_[className];

    auto body = clsExp.list[3];

    for (auto i = 1; i < body.list.size(); i++) {
        auto member = body.list[i];
        if (isVar(member)) {
            auto varNameDecl = member.list[1];
            auto fieldName = excrateVarName(varNameDecl);
            auto fieldType = excrateVarType(varNameDecl);

            classInfo->fieldsMap[fieldName] = fieldType;
        } else if (isDef(member)) {
            auto methodName = member.list[1].string;
            auto fnName = className + "_" + methodName;
            classInfo->methodsMap[methodName] = createFunctionProto(fnName, excrateFunType(member), env);
        }
    }

    // create field
    buildClassBody(cls);
}

// build class body
void EvaLLVM::buildClassBody(llvm::StructType *cls) {
    std::string className{cls->getName().data()};

    auto classInfo = &classMap_[className];

    // allocate the vtable to set its type in the body
    // the table itself is populated later in buildVTable
    auto vTableName = className + "_vTable";
    auto vTableType = llvm::StructType::create(*ctx, vTableName);

    auto clsFields = std::vector<llvm::Type *>{vTableType->getPointerTo()};
    for (const auto &field: classInfo->fieldsMap) {
        clsFields.push_back(field.second);
    }

    cls->setBody(clsFields, false);

    // methods
    // TODO
    buildVTable(cls, classInfo);
}

void EvaLLVM::buildVTable(llvm::StructType *cls, ClassInfo *classInfo) {
    std::string className{cls->getName().data()};
    auto vTableName = className + "_vTable";
    auto vTableType = llvm::StructType::getTypeByName(*ctx, vTableName);

    std::vector<llvm::Constant *> vTableMethods;
    std::vector<llvm::Type *> vTableMethodTypes;

    for (const auto &methodInfo: classInfo->methodsMap) {
        vTableMethods.push_back(methodInfo.second);
        vTableMethodTypes.push_back(methodInfo.second->getType());
    }

    vTableType->setBody(vTableMethodTypes);
    auto vTableValue = llvm::ConstantStruct::get(vTableType, vTableMethods);
    createGlobalVar(vTableName, vTableValue);
}