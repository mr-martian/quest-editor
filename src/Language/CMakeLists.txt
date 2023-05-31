cmake_minimum_required(VERSION 3.10)

project(Vellum
	VERSION 0.0.01
	LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
	message("Clang detected")

	add_compile_options(-fcoroutines-ts -stdlib=libstdc++)
	add_compile_options(-Wall -Wextra -pedantic -pedantic-errors
		-Wconversion -Wno-deprecated-declarations -Wuninitialized
		-Wold-style-cast -Wshadow -Wzero-as-null-pointer-constant
		-Wno-mismatched-tags -Wimplicit-fallthrough -Wno-pragmas)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
	message("GCC detected")

	add_compile_options(-fcoroutines)
	add_compile_options(-Wall -Wextra -pedantic -pedantic-errors
		-Wconversion -Wno-deprecated-declarations -Wuninitialized
		-Wold-style-cast -Wshadow -Wzero-as-null-pointer-constant
		-Wno-mismatched-tags -Wimplicit-fallthrough -Wno-pragmas)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
	message("ICC detected")

	add_compile_options(-fcoroutines)
	add_compile_options(-Wall -Wextra -pedantic -pedantic-errors
		-Wconversion -Wno-deprecated-declarations -Wuninitialized
		-Wold-style-cast -Wshadow -Wzero-as-null-pointer-constant
		-Wno-mismatched-tags -Wimplicit-fallthrough -Wno-pragmas)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
	message("MSVC detected")

	add_compile_options(/W3)
endif()

add_executable(interpreter
	ast.hpp token.hpp
	interpreter.cpp parser.cpp scope.cpp tokenizer.cpp ast.cpp expressions.cpp
	lex.yy.cpp lex.yy.h)

add_compile_definitions(
	"TOKEN_HEADER=<${CMAKE_SOURCE_DIR}/src/Language/token.hpp>"
	"LEX_HEADER=<$<TARGET_FILE_DIR:interpreter>/lex.yy.h>")

find_program(REFLEX reflex)
find_library(LREFLEX reflex)
target_link_libraries(interpreter PUBLIC
	"${LREFLEX}"
	LLVMCore LLVMRemarks LLVMBitstreamReader
	LLVMBinaryFormat LLVMSupport LLVMDemangle
	rt dl tinfo pthread m
	"${EXTRA_LIBS}")

add_executable(ast_generator ast_generator.cpp)

# used to add non-installed libraries
target_include_directories(ast_generator
	SYSTEM PUBLIC "${EXTRA_INCLUDES}"
	INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}
	PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_include_directories(interpreter
	SYSTEM PUBLIC "${EXTRA_INCLUDES}"
	INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}
	PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")

add_custom_command(OUTPUT lex.yy.cpp lex.yy.h
	COMMAND "${REFLEX}" --header-file ${CMAKE_SOURCE_DIR}/src/Language/vellum.l
	MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/src/Language/vellum.l)
target_sources(interpreter PRIVATE vellum.l spec.ebnf precedence.txt)
#target_sources(interpreter PRIVATE
#	$<TARGET_FILE_DIR:interpreter>/lex.yy.h
#	$<TARGET_FILE_DIR:interpreter>/lex.yy.cpp)

source_group(lex REGULAR_EXPRESSION ".*\.l")
source_group(gen REGULAR_EXPRESSION ".*\.yy\..*")

source_group(vellum REGULAR_EXPRESSION ".*\.vll")

add_custom_target(sources ALL
	DEPENDS interpreter
	SOURCES "loc_stats.vll" "traits_compare.vll" "bottles.vll" "containers.vll" "hello.vll")
