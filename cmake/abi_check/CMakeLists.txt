cmake_minimum_required(VERSION 3.19...3.23)
project(abi_check LANGUAGES C Fortran)

enable_testing()

add_library(addone OBJECT addone.c)
add_executable(main_f main.f90 $<TARGET_OBJECTS:addone>)
if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set_target_properties(main_f PROPERTIES LINKER_LANGUAGE Fortran)
else()
  set_target_properties(main_f PROPERTIES LINKER_LANGUAGE C)
endif()
add_test(NAME Fortran_main COMMAND main_f)

add_library(addone_f OBJECT addone.f90)

add_executable(main_c main.c $<TARGET_OBJECTS:addone_f>)
set_target_properties(main_c PROPERTIES LINKER_LANGUAGE C)
add_test(NAME C_main COMMAND main_c)

set_tests_properties(Fortran_main C_main PROPERTIES TIMEOUT 10)
