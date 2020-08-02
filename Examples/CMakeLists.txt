cmake_minimum_required(VERSION 3.14)
project(h5fortranExample LANGUAGES Fortran)
enable_testing()

# --- h5fortran template
include(FetchContent)

FetchContent_Declare(h5fortran_proj
  GIT_REPOSITORY https://github.com/geospace-code/h5fortran.git
  GIT_TAG v2.11.1
)

FetchContent_MakeAvailable(h5fortran_proj)

# --- your project
add_executable(example1 example1.f90)
target_link_libraries(example1 h5fortran::h5fortran)
add_test(NAME h5fortran:Example1 COMMAND $<TARGET_FILE:example1>)

add_executable(example2 example2.f90)
target_link_libraries(example2 h5fortran::h5fortran)
add_test(NAME h5fortran:Example2 COMMAND $<TARGET_FILE:example2>)
