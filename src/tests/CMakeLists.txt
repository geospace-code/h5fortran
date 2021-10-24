add_executable(test_minimal test_minimal.f90)
# even though we're not using h5fortran, we're testing that HDF5 was linked
# as part of h5fortran
target_link_libraries(test_minimal PRIVATE h5fortran::h5fortran)
set_target_properties(test_minimal PROPERTIES LABELS unit)
add_test(NAME h5fortran:minimal
  COMMAND $<TARGET_FILE:test_minimal>)
set_tests_properties(h5fortran:minimal PROPERTIES
FIXTURES_SETUP h5lib
TIMEOUT 5
LABELS core
)
if(DLL_PATH)
  if(WIN32)
    set_tests_properties(h5fortran:minimal PROPERTIES ENVIRONMENT "Path=${DLL_PATH}")
    # set_tests_properties(h5fortran:minimal PROPERTIES ENVIRONMENT_MODIFICATION "Path=path_list_prepend:${DLL_PATH}")
  elseif(APPLE)
    set_tests_properties(h5fortran:minimal PROPERTIES ENVIRONMENT "DYLD_LIBRARY_PATH=${DLL_PATH}")
  elseif(UNIX)
    set_tests_properties(h5fortran:minimal PROPERTIES ENVIRONMENT "LD_LIBRARY_PATH=${DLL_PATH}")
  endif()
endif()

function(setup_test names)

set(CI $ENV{CI})

foreach(name IN LISTS names)

add_executable(test_${name} test_${name}.f90)
target_link_libraries(test_${name} PRIVATE h5fortran::h5fortran)
target_compile_options(test_${name} PRIVATE "$<$<Fortran_COMPILER_ID:GNU>:-Wno-compare-reals;-Wno-conversion>")
set_target_properties(test_${name} PROPERTIES LABELS unit)

add_test(NAME h5fortran:${name}
COMMAND $<TARGET_FILE:test_${name}>
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(h5fortran:${name} PROPERTIES
FIXTURES_REQUIRED h5lib
TIMEOUT 10
)
if(DLL_PATH)
  if(WIN32)
    set_tests_properties(h5fortran:${name} PROPERTIES ENVIRONMENT "Path=${DLL_PATH}")
    # set_tests_properties(h5fortran:${name} PROPERTIES ENVIRONMENT_MODIFICATION "Path=path_list_prepend:${DLL_PATH}")
  elseif(APPLE)
    set_tests_properties(h5fortran:${name} PROPERTIES ENVIRONMENT "DYLD_LIBRARY_PATH=${DLL_PATH}")
  elseif(UNIX)
    set_tests_properties(h5fortran:minimal PROPERTIES ENVIRONMENT "LD_LIBRARY_PATH=${DLL_PATH}")
  endif()
endif()


if(${name} MATCHES ".*fail.*")
  set_tests_properties(h5fortran:${name} PROPERTIES
    WILL_FAIL true
    LABELS shaky
    DISABLED $<OR:$<BOOL:${CI}>,$<NOT:$<BOOL:test_shaky>>>
    )
else()
  set_tests_properties(h5fortran:${name} PROPERTIES
    LABELS unit
    )
endif()

endforeach()

endfunction(setup_test)

# --- all other tests
set(testnames array attributes deflate destructor error exist module layout lt scalar shape string
fail_read_size_mismatch fail_read_rank_mismatch fail_nonexist_variable)

setup_test("${testnames}")

set_tests_properties(h5fortran:shape PROPERTIES
FIXTURES_SETUP h5shape
)

set(shape_file ${PROJECT_BINARY_DIR}/test_shape.h5)
# --- Python h5py

find_package(Python3 COMPONENTS Interpreter)
if(NOT DEFINED h5py_ok)
  execute_process(COMMAND ${Python3_EXECUTABLE} -c "import h5py"
    RESULT_VARIABLE h5py_code
    TIMEOUT 15)
  if(h5py_code EQUAL 0)
    set(h5py_ok true CACHE BOOL "h5py OK")
  else()
    set(h5py_ok false CACHE BOOL "h5py not OK")
  endif()
endif()

add_test(NAME h5fortran:PythonShape
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_shape.py ${shape_file})

set_tests_properties(h5fortran:PythonShape PROPERTIES
REQUIRED_FILES ${shape_file}
FIXTURES_REQUIRED "h5lib;h5shape"
TIMEOUT 15
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

# --- Matlab HDF5

if(matlab)
find_package(Matlab COMPONENTS MAIN_PROGRAM)
if(Matlab_FOUND)
  set(matlab_cmd "i=h5info('${shape_file}', '/d7').Dataspace.Size; assert(all(i == [2, 1, 3, 4, 7, 6, 5]))")

  add_test(NAME h5fortran:MatlabShape COMMAND ${Matlab_MAIN_PROGRAM} -batch ${matlab_cmd})

  set_tests_properties(h5fortran:MatlabShape PROPERTIES
    TIMEOUT 90
    LABELS shaky
    DEPENDS h5fortran:shape
    REQUIRED_FILES ${shape_file}
    FIXTURES_REQUIRED h5lib)

endif(Matlab_FOUND)
endif(matlab)

# --- h5ls

find_program(h5ls NAMES h5ls)
add_test(NAME h5fortran:h5ls COMMAND ${h5ls} ${shape_file}/d7)

set_tests_properties(h5fortran:h5ls PROPERTIES
TIMEOUT 10
REQUIRED_FILES ${shape_file}
FIXTURES_REQUIRED h5lib
DEPENDS h5fortran:shape
PASS_REGULAR_EXPRESSION "{5, 6, 7, 4, 3, 1, 2}"
DISABLED $<NOT:$<BOOL:${h5ls}>>
)
