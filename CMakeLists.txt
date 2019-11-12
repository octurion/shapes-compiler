cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(shapes)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

find_package(FLEX REQUIRED 2.6.0)
find_package(BISON REQUIRED 3.0.4)
find_package(LLVM REQUIRED 3.9)

set(FLEX_BISON_GENFILES_DIR ${CMAKE_CURRENT_BINARY_DIR}/gen)

flex_target(lexer src/lexer.l
    ${FLEX_BISON_GENFILES_DIR}/lexer.cpp
    DEFINES_FILE ${FLEX_BISON_GENFILES_DIR}/lexer.yy.h)
bison_target(parser src/parser.y
    ${FLEX_BISON_GENFILES_DIR}/parser.cpp
    DEFINES_FILE ${FLEX_BISON_GENFILES_DIR}/parser.tab.h
    COMPILE_FLAGS "-Wall -v")
add_flex_bison_dependency(lexer parser)

add_library(shapes_parser ${FLEX_lexer_OUTPUTS} ${BISON_parser_OUTPUTS})
target_include_directories(shapes_parser PUBLIC ${FLEX_BISON_GENFILES_DIR} include)

llvm_map_components_to_libnames(LLVM_LIBS support core irreader)

add_executable(shapes src/main.cpp)

target_compile_options(shapes PRIVATE -Wall -Wextra -Wpedantic -fdiagnostics-color=always)
# target_compile_definitions(shapes PRIVATE ${LLVM_DEFINITIONS})
# target_include_directories(shapes SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})
target_include_directories(shapes PRIVATE include)

target_link_libraries(shapes PRIVATE ${LLVM_LIBS})
target_link_libraries(shapes PRIVATE shapes_parser)
