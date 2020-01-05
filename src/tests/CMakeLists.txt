if(UNIX)
  set(tmpdir $ENV{TMPDIR})
else()
  set(tmpdir $ENV{TEMP})
endif()

add_executable(test_minimal test_minimal.f90)
target_link_libraries(test_minimal PRIVATE h5fortran ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES})
target_include_directories(test_minimal PRIVATE ${CMAKE_BINARY_DIR} ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
add_test(NAME Minimal COMMAND $<TARGET_FILE:test_minimal>)
set_tests_properties(Minimal PROPERTIES
  TIMEOUT 10)

add_executable(test_errors test_error.f90)
target_link_libraries(test_errors PRIVATE h5fortran ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES})
target_include_directories(test_errors PRIVATE ${CMAKE_BINARY_DIR} ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
add_test(NAME ErrorCheck COMMAND $<TARGET_FILE:test_errors> ${tmpdir})
set_tests_properties(ErrorCheck PROPERTIES
  TIMEOUT 10
  SKIP_RETURN_CODE 77)

add_executable(testh5 test_hdf5_ifc.f90)
target_link_libraries(testh5 PRIVATE h5fortran ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES})
target_include_directories(testh5 PRIVATE ${CMAKE_BINARY_DIR} ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(testh5 PRIVATE -Wno-compare-reals)
endif()

add_test(NAME h5oo COMMAND $<TARGET_FILE:testh5> ${tmpdir})
set_tests_properties(h5oo PROPERTIES
  FIXTURES_SETUP h5files
  TIMEOUT 15
  SKIP_RETURN_CODE 77)

set(testfile ${tmpdir}/p5.h5)

# ====
add_executable(test_shape test_shape.f90)
target_link_libraries(test_shape PRIVATE h5fortran ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES})
target_include_directories(test_shape PRIVATE ${CMAKE_BINARY_DIR} ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})

add_test(NAME ShapeCheck COMMAND $<TARGET_FILE:test_shape> ${testfile} /group69/flux_node)
set_tests_properties(ShapeCheck PROPERTIES
  FIXTURES_REQUIRED h5files
  REQUIRED_FILES ${testfile}
  TIMEOUT 15
  RUN_SERIAL true
  SKIP_RETURN_CODE 77)

# ---
if(test_shaky)

find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  add_test(NAME PythonShapeCheck
    COMMAND Python3::Interpreter test_shape.py ${testfile} /group69/flux_node
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
  set_tests_properties(PythonShapeCheck PROPERTIES
    FIXTURES_REQUIRED h5files
    REQUIRED_FILES ${testfile}
    TIMEOUT 30
    RUN_SERIAL true)
endif(Python3_FOUND)

endif(test_shaky)
