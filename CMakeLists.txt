cmake_minimum_required(VERSION 3.10)

# Set the project name
project(EvaLang)

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)
#Find the libgc
find_library(GC_LIB gc REQUIRED)

# Set the compiler to clang++-17
set(CMAKE_CXX_COMPILER "/usr/local/bin/clang++-17")

# add include directories
include_directories(/usr/include/gc)

# Set the LLVM include directories and libraries
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Set the source files directory
set(SOURCE_DIR "${PROJECT_SOURCE_DIR}/src")

# Collect all source files
file(GLOB_RECURSE SOURCES "${SOURCE_DIR}/*.cpp")
file(GLOB_RECURSE HEADERS "${SOURCE_DIR}/*.h")

# Add the executable
add_executable(EvaLang main.cpp ${SOURCES} ${HEADERS})

#link the libgc
target_link_libraries(EvaLang ${GC_LIB})

# Link the LLVM libraries
llvm_map_components_to_libnames(LLVM_LIBS core support)
target_link_libraries(EvaLang ${LLVM_LIBS})

target_compile_options(EvaLang PRIVATE -fstandalone-debug)

# 生成的 IR 文件（使用 .ll 文件）
add_custom_command(
    OUTPUT output.ll
    COMMAND ${OUTPUT_EXECUTABLE} > output.ll
    DEPENDS ${OUTPUT_EXECUTABLE}
)

# 添加目标：编译带 GC 的 IR
add_custom_target(
    compile_gc
    COMMAND clang++ -O3 -I/usr/include/gc output.ll /usr/lib/x86_64-linux-gnu/libgc.a -o output
    DEPENDS output.ll
)

# 打印执行结果
add_custom_target(
    run
    COMMAND ./output  && printf "\\n"
    DEPENDS compile_gc
)
