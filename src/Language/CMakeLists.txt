cmake_minimum_required(VERSION 3.23)

project(Vellum
	VERSION 0.0.01
	LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)

if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
	message("Clang detected")
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

find_program(REFLEX reflex)
find_library(LREFLEX reflex REQUIRED)

add_custom_command(
	OUTPUT lex.yy.cpp lex.yy.h
	COMMAND "${REFLEX}" --header-file "${CMAKE_CURRENT_SOURCE_DIR}/vellum.l"
	MAIN_DEPENDENCY vellum.l
	DEPENDS "${REFLEX}" token.hpp
	VERBATIM
)

add_custom_command(
	OUTPUT lex.attr.cpp lex.attr.h
	COMMAND "${REFLEX}" --prefix=attr --header-file
		--lexer=AttrLexer "${CMAKE_CURRENT_SOURCE_DIR}/attr.l"
	MAIN_DEPENDENCY attr.l
	DEPENDS "${REFLEX}" token.hpp
	VERBATIM
)

add_executable(ast_generator)
target_sources(ast_generator PRIVATE ast_generator.cpp)
target_include_directories(ast_generator PUBLIC "${EXTRA_INCLUDES}")

add_library(lexer OBJECT)
target_sources(lexer
	PRIVATE
		vellum.l attr.l
		${CMAKE_CURRENT_BINARY_DIR}/lex.yy.cpp
		${CMAKE_CURRENT_BINARY_DIR}/lex.attr.cpp
	PUBLIC
		FILE_SET HEADERS
		FILES
			token.hpp
	INTERFACE
		FILE_SET generatedHeaders
		TYPE HEADERS
		BASE_DIRS
			${CMAKE_CURRENT_BINARY_DIR}
		FILES
			${CMAKE_CURRENT_BINARY_DIR}/lex.yy.h
			${CMAKE_CURRENT_BINARY_DIR}/lex.attr.h
)
target_include_directories(lexer PUBLIC "${EXTRA_INCLUDES}")

add_executable(interpreter)
target_sources(interpreter
	PUBLIC
		FILE_SET HEADERS
		FILES
			ast.hpp token.hpp
	PRIVATE
		interpreter.cpp ast.cpp	expressions.cpp parser.cpp scope.cpp
		tokenizer.cpp
	PRIVATE
		README.md spec.ebnf precedence.txt
)
target_include_directories(interpreter PUBLIC "${EXTRA_INCLUDES}")
target_link_libraries(interpreter
	PRIVATE lexer
	PUBLIC "${LREFLEX}"
)

source_group(lex REGULAR_EXPRESSION ".*\.l")
source_group(vellum REGULAR_EXPRESSION ".*\.vll")

add_custom_target(vellum ALL
	DEPENDS interpreter
	SOURCES
		"bottles.vll" "containers.vll" "hello.vll" "loc_stats.vll"
		"traits_compare.vll"
)
