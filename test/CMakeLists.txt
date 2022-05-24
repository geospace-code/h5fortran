set_directory_properties(PROPERTIES LABELS h5fortran)

# --- fundamental HDF5 library check

add_executable(test_minimal test_minimal.f90)
# even though we're not using h5fortran, we're testing that HDF5 was linked
# as part of h5fortran
target_link_libraries(test_minimal PRIVATE h5fortran::h5fortran)
set_target_properties(test_minimal PROPERTIES LABELS unit)
add_test(NAME minimal COMMAND test_minimal)
set_tests_properties(minimal PROPERTIES
FIXTURES_SETUP h5lib
LABELS core
)

# --- h5fortran unit tests

function(setup_test names)

set(CI $ENV{CI})

foreach(name IN LISTS names)

add_executable(test_${name} test_${name}.f90)
target_link_libraries(test_${name} PRIVATE h5fortran::h5fortran)
target_compile_options(test_${name} PRIVATE "$<$<Fortran_COMPILER_ID:GNU>:-Wno-compare-reals;-Wno-conversion>")
set_target_properties(test_${name} PROPERTIES LABELS unit)

if(${name} STREQUAL string_read)
  add_test(NAME ${name} COMMAND test_${name} ${string_file})
else()
  add_test(NAME ${name} COMMAND test_${name})
endif()
set_tests_properties(${name} PROPERTIES
FIXTURES_REQUIRED h5lib
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

if(${name} MATCHES ".*fail.*")
  set_tests_properties(${name} PROPERTIES
  WILL_FAIL true
  LABELS shaky
  DISABLED $<OR:$<BOOL:${CI}>,$<NOT:$<BOOL:test_shaky>>>
  )
else()
  set_tests_properties(${name} PROPERTIES
  LABELS unit
  )
endif()

endforeach()

endfunction(setup_test)

# --- setup unit tests

cmake_path(SET shape_file ${CMAKE_CURRENT_BINARY_DIR}/test_shape.h5)
cmake_path(SET string_file ${CMAKE_CURRENT_BINARY_DIR}/test_string_py.h5)

set(test_names array attributes cast deflate_write deflate_read deflate_props destructor exist fill
groups layout lt scalar shape string string_read version write
fail_read_size_mismatch fail_read_rank_mismatch fail_nonexist_variable)

setup_test("${test_names}")

set_tests_properties(write PROPERTIES FIXTURES_SETUP test_files)

set_tests_properties(shape PROPERTIES FIXTURES_SETUP h5shape)

set_tests_properties(layout PROPERTIES
FIXTURES_REQUIRED test_files
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/test_write.h5
)

set_tests_properties(deflate_write PROPERTIES
FIXTURES_SETUP deflate_files
)

set_tests_properties(deflate_props deflate_read PROPERTIES
FIXTURES_REQUIRED deflate_files
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5
)

if(ENABLE_COVERAGE)
setup_target_for_coverage_gcovr_html(
NAME coverage
EXECUTABLE ${CMAKE_CTEST_COMMAND}
)
endif()

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
  set_tests_properties(${test_names} PROPERTIES
  ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
  )
endif()

# --- Python h5py
find_package(Python COMPONENTS Interpreter)
if(NOT DEFINED h5py_ok)
  execute_process(COMMAND ${Python_EXECUTABLE} -c "import h5py"
  RESULT_VARIABLE h5py_code
  )
  if(h5py_code EQUAL 0)
    set(h5py_ok true CACHE BOOL "h5py OK")
  else()
    set(h5py_ok false CACHE BOOL "h5py not OK")
  endif()
endif()

set_tests_properties(string_read PROPERTIES
FIXTURES_REQUIRED h5str
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

add_test(NAME PythonShape
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_shape.py ${shape_file}
)

set_tests_properties(PythonShape PROPERTIES
REQUIRED_FILES ${shape_file}
FIXTURES_REQUIRED "h5lib;h5shape"
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

add_test(NAME PythonString
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_string.py ${string_file}
)

set_tests_properties(PythonString PROPERTIES
FIXTURES_REQUIRED h5lib
FIXTURES_SETUP h5str
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

# --- Matlab HDF5

if(matlab)
find_package(Matlab COMPONENTS MAIN_PROGRAM)
if(Matlab_FOUND)
  set(matlab_cmd "i=h5info('${shape_file}', '/d7').Dataspace.Size; assert(all(i == [2, 1, 3, 4, 7, 6, 5]))")

  add_test(NAME MatlabShape COMMAND ${Matlab_MAIN_PROGRAM} -batch ${matlab_cmd})

  set_tests_properties(MatlabShape PROPERTIES
  LABELS shaky
  DEPENDS shape
  REQUIRED_FILES ${shape_file}
  FIXTURES_REQUIRED "h5lib;h5shape"
  )

endif(Matlab_FOUND)
endif(matlab)

# --- h5ls

find_program(h5ls NAMES h5ls)
add_test(NAME h5ls COMMAND ${h5ls} ${shape_file}/d7)

set_tests_properties(h5ls PROPERTIES
REQUIRED_FILES ${shape_file}
FIXTURES_REQUIRED "h5lib;h5shape"
DEPENDS shape
PASS_REGULAR_EXPRESSION "{5, 6, 7, 4, 3, 1, 2}"
DISABLED $<NOT:$<BOOL:${h5ls}>>
)


get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_tests_properties(${test_names} PROPERTIES TIMEOUT 60)
