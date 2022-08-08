cmake_minimum_required(VERSION 3.11...3.25)

project(h5fortranExample
LANGUAGES C CXX Fortran)

enable_testing()

find_package(h5fortran CONFIG REQUIRED)

# --- Fortran interface for examples
add_library(fortran_interface fortran_interface.f90)
target_link_libraries(fortran_interface PRIVATE h5fortran::h5fortran)

add_executable(ex_fcn ex_fcn.f90)
target_link_libraries(ex_fcn PRIVATE h5fortran::h5fortran)
add_test(NAME Fortran_fcn COMMAND ex_fcn)

add_executable(ex_oo ex_oo.f90)
target_link_libraries(ex_oo PRIVATE h5fortran::h5fortran)
add_test(NAME Fortran_oo COMMAND ex_oo)

add_executable(repeat_char_read char_repeat_read.f90)
target_link_libraries(repeat_char_read PRIVATE h5fortran::h5fortran)

add_executable(c_fcn ex_fcn.c)
target_link_libraries(c_fcn PRIVATE fortran_interface)
target_compile_features(c_fcn PRIVATE c_std_99)
# https://en.cppreference.com/w/c/types/integer
add_test(NAME C_fcn COMMAND c_fcn)

add_executable(cpp_fcn ex_fcn.cpp)
target_link_libraries(cpp_fcn PRIVATE fortran_interface)
target_compile_features(cpp_fcn PRIVATE cxx_std_11)
# https://en.cppreference.com/w/cpp/types/integer
add_test(NAME CPP_fcn COMMAND cpp_fcn)

# properties
get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)
set_property(TEST ${test_names} PROPERTY TIMEOUT 10)
set_property(TEST ${test_names} PROPERTY WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
set_property(TEST ${test_names} PROPERTY
  ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin;PATH=path_list_append:${PROJECT_BINARY_DIR}/bin;PATH=path_list_append:${h5fortran_DIR}/../bin"
  )
endif()
