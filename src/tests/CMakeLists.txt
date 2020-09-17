add_executable(test_minimal test_minimal.f90)
# even though we're not using h5fortran, we're testing that HDF5 was linked
# as part of h5fortran
target_link_libraries(test_minimal PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:minimal
  COMMAND $<TARGET_FILE:test_minimal>
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
set_tests_properties(h5fortran:minimal PROPERTIES FIXTURES_SETUP h5lib)

# --- all other tests
foreach(t array attributes deflate error exist module lt scalar shape string
          fail_read_size_mismatch fail_read_rank_mismatch fail_nonexist_variable fail_unknown_read fail_unknown_write)

  add_executable(test_${t} test_${t}.f90)
  target_link_libraries(test_${t} PRIVATE h5fortran::h5fortran)
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
    target_compile_options(test_${t} PRIVATE -Wno-compare-reals)
  endif()

  add_test(NAME h5fortran:${t}
    COMMAND $<TARGET_FILE:test_${t}>
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
  set_tests_properties(h5fortran:${t} PROPERTIES FIXTURES_REQUIRED h5lib)

endforeach()

foreach(t fail_read_size_mismatch fail_read_rank_mismatch fail_nonexist_variable fail_unknown_read fail_unknown_write)
  set_tests_properties(h5fortran:${t} PROPERTIES WILL_FAIL true)
endforeach()


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
