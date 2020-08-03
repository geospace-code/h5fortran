set(tmpdir ${CMAKE_CURRENT_BINARY_DIR})

add_executable(test_minimal test_minimal.f90)
target_link_libraries(test_minimal PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:minimal COMMAND $<TARGET_FILE:test_minimal> ${tmpdir})
set_tests_properties(h5fortran:minimal PROPERTIES FIXTURES_SETUP h5lib)

add_executable(test_attributes test_attributes.f90)
target_link_libraries(test_attributes PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:attributes COMMAND $<TARGET_FILE:test_attributes> ${tmpdir})
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(test_attributes PRIVATE -Wno-compare-reals)
endif()
set_tests_properties(h5fortran:attributes PROPERTIES
  SKIP_RETURN_CODE 77
  FIXTURES_REQUIRED h5lib)

add_executable(test_errors test_error.f90)
target_link_libraries(test_errors h5fortran::h5fortran)
add_test(NAME h5fortran:ErrorCheck COMMAND $<TARGET_FILE:test_errors> ${tmpdir})
set_tests_properties(h5fortran:ErrorCheck PROPERTIES
  SKIP_RETURN_CODE 77
  FIXTURES_REQUIRED h5lib)

add_executable(test_exist test_exist.f90)
target_link_libraries(test_exist h5fortran::h5fortran)
add_test(NAME h5fortran:test_exist COMMAND $<TARGET_FILE:test_exist> ${tmpdir})
set_tests_properties(h5fortran:test_exist PROPERTIES
  SKIP_RETURN_CODE 77
  FIXTURES_REQUIRED h5lib)

add_executable(test_string test_string.f90)
target_link_libraries(test_string h5fortran::h5fortran)
add_test(NAME h5fortran:test_string COMMAND $<TARGET_FILE:test_string> ${tmpdir})
set_tests_properties(h5fortran:test_string PROPERTIES
  SKIP_RETURN_CODE 77
  FIXTURES_REQUIRED h5lib)

add_executable(testh5
test_hdf5_ifc.f90 test_lt.f90 test_array.f90 test_scalar.f90)
target_link_libraries(testh5 PRIVATE h5fortran::h5fortran)
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(testh5 PRIVATE -Wno-compare-reals)
endif()
add_test(NAME h5fortran:module COMMAND $<TARGET_FILE:testh5> ${tmpdir})
set_tests_properties(h5fortran:module PROPERTIES
  FIXTURES_REQUIRED h5lib
  FIXTURES_SETUP h5files
  SKIP_RETURN_CODE 77)

add_executable(test_deflate test_deflate.f90)
target_link_libraries(test_deflate PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:deflate COMMAND $<TARGET_FILE:test_deflate> ${tmpdir})
set_tests_properties(h5fortran:deflate PROPERTIES
  SKIP_RETURN_CODE 77
  FIXTURES_REQUIRED h5lib)

set(testfile ${tmpdir}/p5.h5)

add_executable(test_shape test_shape.f90)
target_link_libraries(test_shape PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:shape_check COMMAND $<TARGET_FILE:test_shape> ${tmpdir})
set_tests_properties(h5fortran:shape_check PROPERTIES
  FIXTURES_REQUIRED h5lib
  SKIP_RETURN_CODE 77)

if(WIN32 AND MSVC)
  foreach(t h5fortran:minimal h5fortran:ErrorCheck h5fortran:test_exist h5fortran:module h5fortran:deflate h5fortran:ShapeCheck)
    set_tests_properties(${t} PROPERTIES ENVIRONMENT "PATH=${HDF5_ROOT}/bin;$ENV{PATH}")
  endforeach()
endif()

# --- shaky tests
if(NOT test_shaky)
  return()
endif()

find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  add_test(NAME h5fortran:PythonShapeCheck
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_shape.py ${testfile} /group69/flux_node)
  set_tests_properties(h5fortran:PythonShapeCheck PROPERTIES
    FIXTURES_REQUIRED "h5lib;h5files"
    REQUIRED_FILES ${testfile}
    TIMEOUT 60
    RUN_SERIAL true)
endif(Python3_FOUND)
