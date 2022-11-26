cmake_minimum_required(VERSION 3.10)

project(Vellum
	VERSION 0.0.01
	LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# warning options
if (MSVC)
    add_compile_options(/W3)
else()
    add_compile_options(-Wall -Wextra -pedantic -pedantic-errors
		-Wconversion -Wno-deprecated-declarations
		-Wold-style-cast -Wshadow -Wzero-as-null-pointer-constant
		-Wno-mismatched-tags -Wimplicit-fallthrough -Wno-pragmas)
endif()

add_executable(ast_generator ast_generator.cpp)

add_executable(interpreter interpreter.cpp parser.cpp lex.yy.cpp ast.hpp
	tokenizer.cpp)

set_source_files_properties(lex.yy.cpp
	PROPERTIES COMPILE_DEFINITIONS
	"AST_HEADER=\"${CMAKE_SOURCE_DIR}/src/Language/ast.hpp\"")

target_link_libraries(interpreter PUBLIC
	reflex
	LLVMCore LLVMRemarks LLVMBitstreamReader
	LLVMBinaryFormat LLVMSupport LLVMDemangle
	rt dl tinfo pthread m
	"${EXTRA_LIBS}")

# used to add non-installed libraries
target_include_directories(interpreter
	SYSTEM PUBLIC "${EXTRA_INCLUDES}")

find_program(REFLEX reflex)
add_custom_command(OUTPUT lex.yy.cpp lex.yy.h
	COMMAND "${REFLEX}" --header-file ${CMAKE_SOURCE_DIR}/src/Language/vellum.l
	MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/src/Language/vellum.l)
target_sources(interpreter PRIVATE vellum.l spec.ebnf precedence.txt)

source_group(lex REGULAR_EXPRESSION ".*\.l")

source_group(vellum REGULAR_EXPRESSION ".*\.vll")

add_custom_target(sources ALL
	DEPENDS interpreter
	SOURCES "loc_stats.vll" "traits_compare.vll")
