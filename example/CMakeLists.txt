cmake_minimum_required(VERSION 3.22...3.27)

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

## VTK HDF5 write example
add_executable(vtk_write vtk_write.f90)
target_link_libraries(vtk_write PRIVATE h5fortran::h5fortran)
add_test(NAME VTK COMMAND vtk_write ${CMAKE_CURRENT_BINARY_DIR}/vtk.hdf)

## C, C++ examples

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
set_property(TEST ${test_names} PROPERTY WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

if(WIN32)
set_property(TEST ${test_names} PROPERTY
  ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${h5fortran_DIR}/../bin"
  )
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
set_property(TEST ${test_names} PROPERTY
  ENVIRONMENT_MODIFICATION "LD_LIBRARY_PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../lib;LD_LIBRARY_PATH=path_list_append:${h5fortran_DIR}/../lib"
  )
endif()
