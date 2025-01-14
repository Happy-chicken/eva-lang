cmake_minimum_required(VERSION 3.10)

# Set the project name
project(EvaLang)


set(LLVM_DIR "/usr/lib/llvm-14/cmake")
# Find LLVM package
find_package(LLVM REQUIRED CONFIG)
#Find the libgc
find_library(GC_LIB gc REQUIRED)

# Set the compiler to clang++-14
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

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION} at ${LLVM_DIR}")
