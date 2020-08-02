cmake_minimum_required(VERSION 3.14)
project(h5fortranExample
LANGUAGES C Fortran)
enable_testing()

# --- h5fortran template
include(FetchContent)

FetchContent_Declare(h5fortran_proj
  GIT_REPOSITORY https://github.com/geospace-code/h5fortran.git
  GIT_TAG v2.11.1
)

FetchContent_MakeAvailable(h5fortran_proj)

# this helps linters e.g. Visual Studio Intellicode work properly
set(CMAKE_EXPORT_COMPILE_COMMANDS on)

# --- example 1
add_executable(example1 example1.f90)
target_link_libraries(example1 h5fortran::h5fortran)
add_test(NAME h5fortran:Example1 COMMAND $<TARGET_FILE:example1>)

# --- example 2
add_executable(example2 example2.f90)
target_link_libraries(example2 h5fortran::h5fortran)
add_test(NAME h5fortran:Example2 COMMAND $<TARGET_FILE:example2>)

# --- example 3
add_library(example3_f example3.f90)
target_link_libraries(example3_f PRIVATE h5fortran::h5fortran)
add_executable(example3 example3.c)
target_link_libraries(example3 example3_f)
