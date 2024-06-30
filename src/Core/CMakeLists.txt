cmake_minimum_required(VERSION 3.10)

if (CMAKE_GENERATOR MATCHES "Ninja")
file(
    WRITE "${CMAKE_BINARY_DIR}/GNUMakeRulesOverwrite.cmake"
        "STRING(REPLACE \"-MD\" \"-MMD\" CMAKE_DEPFILE_FLAGS_C \"\${CMAKE_DEPFILE_FLAGS_C}\")\n"
        "STRING(REPLACE \"-MD\" \"-MMD\" CMAKE_DEPFILE_FLAGS_CXX \"\${CMAKE_DEPFILE_FLAGS_CXX}\")\n"
)
set(CMAKE_USER_MAKE_RULES_OVERRIDE "${CMAKE_BINARY_DIR}/GNUMakeRulesOverwrite.cmake" CACHE INTERNAL "")
endif()

project(Quest
	VERSION 0.0.01
	LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
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

add_executable(quest-editor
	core_main.cpp quest_types.hpp rope.hpp utf8_rope.cpp utf8_rope.hpp)

# used to add non-installed libraries
target_include_directories(quest-editor
	SYSTEM PUBLIC "${EXTRA_INCLUDES}"
	INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}
	PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
