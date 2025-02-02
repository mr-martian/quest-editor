cmake_minimum_required(VERSION 3.14)

project(Quest
  VERSION 0.0.01)

include(FetchContent)

FetchContent_Declare(
  reflex
  GIT_REPOSITORY https://github.com/Genivia/RE-flex
  GIT_TAG master
)

FetchContent_Declare(
  kblib
  GIT_REPOSITORY https://github.com/killerbee13/kblib
  GIT_TAG main
)

FetchContent_Declare(
  Asyncpp
  GIT_REPOSITORY https://github.com/asyncpp/asyncpp
  GIT_TAG master
)

FetchContent_Declare(
  TCLAP
  GIT_REPOSITORY https://github.com/mirror/tclap
  GIT_TAG master
)

# fetch Google-test to avoid conflicts
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest
  GIT_TAG v1.14.0
)

FetchContent_Declare(
  GSL
  GIT_REPOSITORY https://github.com/Microsoft/GSL
  GIT_TAG main
)


FetchContent_MakeAvailable(
  reflex
  kblib
  Asyncpp
  TCLAP
  GSL
)

add_subdirectory(src)
source_group(TREE "src")

source_group(DIST "LICENSE.txt" "Readme.md")
