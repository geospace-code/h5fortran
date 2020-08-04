add_executable(test_minimal test_minimal.f90)
target_link_libraries(test_minimal PRIVATE h5fortran::h5fortran)
add_test(NAME h5fortran:minimal COMMAND $<TARGET_FILE:test_minimal>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_tests_properties(h5fortran:minimal PROPERTIES FIXTURES_SETUP h5lib)

foreach(t array attributes deflate error exist module lt scalar shape string)
  add_executable(test_${t} test_${t}.f90)
  target_link_libraries(test_${t} PRIVATE h5fortran::h5fortran)
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
    target_compile_options(test_${t} PRIVATE -Wno-compare-reals)
  endif()

  add_test(NAME h5fortran:${t} COMMAND $<TARGET_FILE:test_${t}> WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
  set_tests_properties(h5fortran:${t} PROPERTIES FIXTURES_REQUIRED h5lib)
  if(WIN32 AND MSVC)
    set_tests_properties(h5fortran:${t} PROPERTIES ENVIRONMENT "PATH=${HDF5_ROOT}/bin;$ENV{PATH}")
  endif()
endforeach()


find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  add_test(NAME h5fortran:check_shape
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_shape.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
  set_tests_properties(h5fortran:check_shape PROPERTIES
    DEPENDS h5fortran:shape
    FIXTURES_REQUIRED h5lib
    TIMEOUT 90)
endif(Python3_FOUND)
