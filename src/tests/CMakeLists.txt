add_executable(test_minimal test_minimal.f90)
target_link_libraries(test_minimal PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:minimal COMMAND $<TARGET_FILE:test_minimal>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:minimal PROPERTIES FIXTURES_SETUP h5lib)

add_executable(test_array test_array.f90)
target_link_libraries(test_array PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:array COMMAND $<TARGET_FILE:test_array>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(test_array PRIVATE -Wno-compare-reals)
endif()
set_tests_properties(h5fortran:array PROPERTIES FIXTURES_REQUIRED h5lib)

add_executable(test_attributes test_attributes.f90)
target_link_libraries(test_attributes PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:attributes COMMAND $<TARGET_FILE:test_attributes>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(test_attributes PRIVATE -Wno-compare-reals)
endif()
set_tests_properties(h5fortran:attributes PROPERTIES FIXTURES_REQUIRED h5lib)

add_executable(test_deflate test_deflate.f90)
target_link_libraries(test_deflate PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:deflate COMMAND $<TARGET_FILE:test_deflate>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:deflate PROPERTIES FIXTURES_REQUIRED h5lib)

add_executable(test_error test_error.f90)
target_link_libraries(test_error h5fortran::h5fortran)
add_test(NAME h5fortran:error COMMAND $<TARGET_FILE:test_error>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:error PROPERTIES FIXTURES_REQUIRED h5lib)

add_executable(test_exist test_exist.f90)
target_link_libraries(test_exist h5fortran::h5fortran)
add_test(NAME h5fortran:test_exist COMMAND $<TARGET_FILE:test_exist>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:test_exist PROPERTIES FIXTURES_REQUIRED h5lib)

add_executable(test_scalar test_scalar.f90)
target_link_libraries(test_scalar h5fortran::h5fortran)
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(test_scalar PRIVATE -Wno-compare-reals)
endif()
add_test(NAME h5fortran:test_scalar COMMAND $<TARGET_FILE:test_scalar>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:test_scalar PROPERTIES FIXTURES_REQUIRED h5lib)

add_executable(test_string test_string.f90)
target_link_libraries(test_string h5fortran::h5fortran)
add_test(NAME h5fortran:test_string COMMAND $<TARGET_FILE:test_string>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:test_string PROPERTIES FIXTURES_REQUIRED h5lib)

add_executable(testh5 test_hdf5_ifc.f90 test_lt.f90)
target_link_libraries(testh5 PRIVATE h5fortran::h5fortran)
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(testh5 PRIVATE -Wno-compare-reals)
endif()
add_test(NAME h5fortran:module COMMAND $<TARGET_FILE:testh5>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:module PROPERTIES FIXTURES_REQUIRED h5lib)

add_executable(test_shape test_shape.f90)
target_link_libraries(test_shape PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:shape
  COMMAND $<TARGET_FILE:test_shape>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:shape PROPERTIES
  FIXTURES_REQUIRED h5lib
  FIXTURES_SETUP h5files)

if(WIN32 AND MSVC)
  foreach(t h5fortran:minimal h5fortran:error h5fortran:test_exist h5fortran:module h5fortran:deflate h5fortran:ShapeCheck)
    set_tests_properties(${t} PROPERTIES ENVIRONMENT "PATH=${HDF5_ROOT}/bin;$ENV{PATH}")
  endforeach()
endif()

find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  add_test(NAME h5fortran:check_shape
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_shape.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
  set_tests_properties(h5fortran:check_shape PROPERTIES
    FIXTURES_REQUIRED h5files
    TIMEOUT 90)
endif(Python3_FOUND)
