cmake_minimum_required(VERSION 3.15)

include(${CMAKE_CURRENT_SOURCE_DIR}/../cmake/policy.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/../cmake/compiler_find.cmake)
find_c_fortran()

project(h5fortranExample
LANGUAGES C CXX Fortran)
enable_testing()

# see if h5fortran is installed; build h5fortran if not found
find_package(h5fortran CONFIG)

if(h5fortran_FOUND)
  include(${h5fortran_DIR}/h5fortranTargets.cmake)
else()
  include(FetchContent)
  FetchContent_Declare(h5fortran_proj
    GIT_REPOSITORY https://github.com/geospace-code/h5fortran.git
    GIT_TAG main)
  FetchContent_MakeAvailable(h5fortran_proj)
endif()

# --- Fortran interface for examples
add_library(fortran_interface fortran_interface.f90)
target_link_libraries(fortran_interface PRIVATE h5fortran::h5fortran)

# --- example 1
add_executable(example1 example1.f90)
target_link_libraries(example1 h5fortran::h5fortran)
add_test(NAME h5fortran:Example1 COMMAND $<TARGET_FILE:example1>
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# --- example 2
add_executable(example2 example2.f90)
target_link_libraries(example2 h5fortran::h5fortran)
add_test(NAME h5fortran:Example2 COMMAND $<TARGET_FILE:example2>
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# --- example 3
add_executable(example3 example3.c)
target_link_libraries(example3 fortran_interface)
target_compile_features(example3 PRIVATE c_std_99)
# https://en.cppreference.com/w/c/types/integer
add_test(NAME h5fortran:Example3 COMMAND $<TARGET_FILE:example3>
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# --- example 4
add_executable(example4 example4.cxx)
target_link_libraries(example4 fortran_interface)
target_compile_features(example3 PRIVATE cxx_std_11)
# https://en.cppreference.com/w/cpp/types/integer
add_test(NAME h5fortran:Example4 COMMAND $<TARGET_FILE:example4>
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
