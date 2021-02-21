add_executable(test_minimal test_minimal.f90)
# even though we're not using h5fortran, we're testing that HDF5 was linked
# as part of h5fortran
target_link_libraries(test_minimal PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:minimal
  COMMAND $<TARGET_FILE:test_minimal>
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
set_tests_properties(h5fortran:minimal PROPERTIES FIXTURES_SETUP h5lib)

function(setup_test names)

set(CI $ENV{CI})

foreach(name IN LISTS names)

if(CI AND ${name} MATCHES ".*fail.*")
  continue()
endif()

add_executable(test_${name} test_${name}.f90)
target_link_libraries(test_${name} PRIVATE h5fortran::h5fortran)
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(test_${name} PRIVATE -Wno-compare-reals)
endif()

add_test(NAME h5fortran:${name}
  COMMAND $<TARGET_FILE:test_${name}>
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
set_tests_properties(h5fortran:${name} PROPERTIES FIXTURES_REQUIRED h5lib)


if(${name} MATCHES ".*fail.*")
  set_tests_properties(h5fortran:${name} PROPERTIES WILL_FAIL true)
endif()

endforeach()

endfunction(setup_test)

# --- all other tests
set(testnames array attributes deflate error exist module layout lt scalar shape string
fail_read_size_mismatch fail_read_rank_mismatch fail_nonexist_variable
fail_unknown_read fail_unknown_write)

setup_test("${testnames}")


find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  add_test(NAME h5fortran:check_shape
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_shape.py ${PROJECT_BINARY_DIR}/test_shape.h5)
  set_tests_properties(h5fortran:check_shape PROPERTIES
    DEPENDS h5fortran:shape
    REQUIRED_FILES ${PROJECT_BINARY_DIR}/test_shape.h5
    FIXTURES_REQUIRED h5lib
    TIMEOUT 90)
endif(Python3_FOUND)
