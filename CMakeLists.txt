cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(shapes)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(LLVM_PATHS "/usr/lib/llvm-8/" CACHE PATH "Path to LLVM installation")

find_package(FLEX REQUIRED 2.6.0)
find_package(BISON REQUIRED 3.0.4)
find_package(LLVM REQUIRED 8.0 CONFIG PATHS ${LLVM_PATHS} NO_DEFAULT_PATH)
llvm_map_components_to_libnames(LLVM_LIBS support core irreader passes target interpreter
    x86asmparser x86asmprinter x86codegen x86desc x86info x86utils)

set(FLEX_BISON_GENFILES_DIR ${CMAKE_CURRENT_BINARY_DIR}/gen)

flex_target(lexer src/lexer.l
    ${FLEX_BISON_GENFILES_DIR}/lexer.cpp
    DEFINES_FILE ${FLEX_BISON_GENFILES_DIR}/lexer.yy.h)
bison_target(parser src/parser.y
    ${FLEX_BISON_GENFILES_DIR}/parser.cpp
    DEFINES_FILE ${FLEX_BISON_GENFILES_DIR}/parser.tab.h
    COMPILE_FLAGS "-Wall -v")
add_flex_bison_dependency(lexer parser)

add_library(shapes_parser
    src/cst.cpp
    src/ast.cpp
    src/common.cpp
    src/semantic_analysis.cpp
    src/ir.cpp)

add_subdirectory(lib/googletest)

target_compile_options(shapes_parser PRIVATE -Wall -Wextra -Wpedantic -fdiagnostics-color=always)
target_compile_definitions(shapes_parser PRIVATE ${LLVM_DEFINITIONS})

target_include_directories(shapes_parser SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})
target_include_directories(shapes_parser PUBLIC include)
target_link_libraries(shapes_parser PRIVATE ${LLVM_LIBS})

add_library(shapes_bison ${FLEX_lexer_OUTPUTS} ${BISON_parser_OUTPUTS})
target_include_directories(shapes_bison SYSTEM PUBLIC ${FLEX_BISON_GENFILES_DIR})
target_link_libraries(shapes_bison PUBLIC shapes_parser)

add_executable(shapes src/main.cpp)

add_executable(shapes_tests testing/tests.cpp)
target_compile_options(shapes_tests PRIVATE -Wall -Wextra -Wpedantic -fdiagnostics-color=always)
target_link_libraries(shapes_tests PRIVATE shapes_parser shapes_bison gtest gmock gtest_main)

target_compile_options(shapes PRIVATE -Wall -Wextra -Wpedantic -fdiagnostics-color=always)
target_link_libraries(shapes PRIVATE shapes_parser shapes_bison)
