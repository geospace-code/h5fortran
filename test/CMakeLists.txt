# AssignProcessToJobObject: (87) The parameter is incorrect.
# Not sure which test this is coming from. On Windows, avoiding parallel run works around this error.

set_property(DIRECTORY PROPERTY LABELS h5fortran)

# workaround flags for tests

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-Wno-compare-reals;-Wno-maybe-uninitialized>")
endif()

# test files

set(attr_file ${CMAKE_CURRENT_BINARY_DIR}/test_attr_py.h5)
set(shape_file ${CMAKE_CURRENT_BINARY_DIR}/test_shape.h5)
set(string_file ${CMAKE_CURRENT_BINARY_DIR}/test_string_py.h5)

# --- fundamental HDF5 library check

add_executable(test_minimal test_minimal.f90)
# even though we're not using h5fortran, we're testing that HDF5 was linked
# as part of h5fortran
target_link_libraries(test_minimal PRIVATE h5fortran::h5fortran)
add_test(NAME minimal COMMAND test_minimal)
set_property(TEST minimal PROPERTY FIXTURES_SETUP minimal)

# --- h5fortran unit tests

function(setup_test names)

set(CI $ENV{CI})

foreach(name IN LISTS names)

add_executable(test_${name} test_${name}.f90)
target_link_libraries(test_${name} PRIVATE h5fortran::h5fortran)

if(${name} STREQUAL "string_read")
  add_test(NAME ${name} COMMAND test_${name} ${string_file})
elseif(${name} STREQUAL "attributes_read")
  add_test(NAME ${name} COMMAND test_${name} ${attr_file})
else()
  add_test(NAME ${name} COMMAND test_${name})
endif()

if(${name} MATCHES ".*fail.*")
  set_tests_properties(${name} PROPERTIES
  WILL_FAIL true
  LABELS shaky
  DISABLED $<OR:$<BOOL:${CI}>,$<NOT:$<BOOL:test_shaky>>>
  )
endif()

endforeach()

endfunction(setup_test)

# --- setup unit tests

set(test_names array attributes attributes_read
cast deflate_write deflate_read deflate_props destructor exist
groups layout lt scalar shape string string_read version write
fail_read_size_mismatch fail_read_rank_mismatch fail_nonexist_variable)
if(HAVE_IEEE_ARITH)
  list(APPEND test_names fill)
endif()

setup_test("${test_names}")

set_property(TEST write PROPERTY FIXTURES_SETUP test_files)

set_property(TEST shape PROPERTY FIXTURES_SETUP h5shape)

set_property(TEST layout PROPERTY FIXTURES_REQUIRED test_files)
set_property(TEST layout PROPERTY REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/test_write.h5)

set_property(TEST deflate_write PROPERTY FIXTURES_SETUP deflate_files)

set_property(TEST deflate_props deflate_read PROPERTY FIXTURES_REQUIRED deflate_files)
set_property(TEST deflate_props deflate_read PROPERTY REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5)

if(ENABLE_COVERAGE)
setup_target_for_coverage_gcovr_html(
NAME coverage
EXECUTABLE ${CMAKE_CTEST_COMMAND}
)
endif()

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
  set_property(TEST ${test_names} PROPERTY
  ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
  )
endif()

# --- Python h5py
set(Python_FIND_VIRTUALENV STANDARD)
# use order in PATH to pick Python interpreter
# this avoids unexpected old system Python getting used IF conda environment has been activated
find_package(Python COMPONENTS Interpreter)

if(NOT DEFINED h5py_ok)
  execute_process(COMMAND ${Python_EXECUTABLE} -c "import h5py"
  RESULT_VARIABLE ret
  ERROR_VARIABLE err
  )
  message(VERBOSE "${ret} ${err}")
  if(ret EQUAL 0)
    set(h5py_ok true CACHE BOOL "h5py OK")
  else()
    set(h5py_ok false CACHE BOOL "h5py not OK")
  endif()
endif()

set_property(TEST string_read PROPERTY FIXTURES_REQUIRED h5str)
set_property(TEST string_read PROPERTY REQUIRED_FILES ${string_file})

# --- attributes

add_test(NAME PythonAttributes
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_attributes.py ${attr_file}
)

set_property(TEST PythonAttributes PROPERTY FIXTURES_SETUP h5attr)

set_property(TEST attributes_read PROPERTY FIXTURES_REQUIRED h5attr)
set_property(TEST attributes_read PROPERTY REQUIRED_FILES ${attr_file})

# --- shape

add_test(NAME PythonShape
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_shape.py ${shape_file}
)

set_property(TEST PythonShape PROPERTY REQUIRED_FILES ${shape_file})
set_property(TEST PythonShape PROPERTY FIXTURES_REQUIRED h5shape)

# --- String
add_test(NAME PythonString
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_string.py ${string_file}
)

set_property(TEST PythonString PROPERTY FIXTURES_SETUP h5str)

set_property(TEST string_read PythonAttributes PythonShape PythonString attributes_read PROPERTY DISABLED $<NOT:$<BOOL:${h5py_ok}>>)

set_property(TEST PythonAttributes PythonShape PythonString PROPERTY LABELS python)

# --- Matlab HDF5

if(matlab)
find_package(Matlab COMPONENTS MAIN_PROGRAM REQUIRED)

set(matlab_cmd "i=h5info('${shape_file}', '/d7').Dataspace.Size; assert(all(i == [2, 1, 3, 4, 7, 6, 5]))")

add_test(NAME MatlabShape COMMAND ${Matlab_MAIN_PROGRAM} -batch ${matlab_cmd})

set_tests_properties(MatlabShape PROPERTIES
LABELS "shaky;matlab"
DEPENDS shape
REQUIRED_FILES ${shape_file}
FIXTURES_REQUIRED h5shape
)

endif(matlab)

# --- h5ls

find_program(h5ls NAMES h5ls)
add_test(NAME h5ls COMMAND ${h5ls} ${shape_file}/d7)

set_tests_properties(h5ls PROPERTIES
REQUIRED_FILES ${shape_file}
FIXTURES_REQUIRED h5shape
DEPENDS shape
PASS_REGULAR_EXPRESSION "{5, 6, 7, 4, 3, 1, 2}"
DISABLED $<NOT:$<BOOL:${h5ls}>>
)


get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_property(TEST ${test_names} PROPERTY WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

foreach(n IN LISTS test_names)
  if(n STREQUAL "minimal")
    continue()
  endif()

  get_property(f TEST ${n} PROPERTY FIXTURES_REQUIRED)
  set_property(TEST ${n} PROPERTY FIXTURES_REQUIRED "minimal;${f}")
endforeach()
