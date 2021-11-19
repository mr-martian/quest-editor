cmake_minimum_required(VERSION 3.10)

project(Vellum
	VERSION 0.0.01
	LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(interpreter interpreter.cpp parser.cpp lex.yy.cpp ast.hpp)

target_link_libraries(interpreter PUBLIC
	LLVMCore LLVMRemarks LLVMBitstreamReader
	LLVMBinaryFormat LLVMSupport LLVMDemangle
	rt dl tinfo pthread m
	"${EXTRA_LIBS}")

target_include_directories(interpreter PUBLIC "${EXTRA_INCLUDES}")

find_program(REFLEX reflex)
add_custom_command(OUTPUT lex.yy.cpp
	COMMAND "${REFLEX}" vellum.l
	DEPENDS vellum.l)
target_sources(interpreter PRIVATE vellum.l spec.ebnf precedence.txt)

source_group(lex REGULAR_EXPRESSION ".*\.l")

add_custom_target(sources ALL
	DEPENDS interpreter
	SOURCES "loc_stats.vll")
