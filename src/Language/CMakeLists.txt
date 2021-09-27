cmake_minimum_required(VERSION 3.10)

project(Vellum VERSION 0.0.01)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(interpreter interpreter.cpp)

target_link_libraries(interpreter PUBLIC
	LLVMCore LLVMRemarks LLVMBitstreamReader
	LLVMBinaryFormat LLVMSupport LLVMDemangle
	rt dl tinfo pthread m
	"${EXTRA_LIBS}")

target_include_directories(interpreter PUBLIC "${EXTRA_INCLUDES}")
