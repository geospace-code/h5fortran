add_executable(testh5 test_hdf5_ifc.f90)
target_link_libraries(testh5 PRIVATE h5fortran)
target_include_directories(testh5 PRIVATE ${CMAKE_BINARY_DIR})
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  target_compile_options(testh5 PRIVATE -Wno-compare-reals)
endif()

add_test(NAME h5oo COMMAND testh5)
set_tests_properties(h5oo PROPERTIES
  FIXTURES_SETUP h5files
  TIMEOUT 15)

set(testfile ${CMAKE_CURRENT_BINARY_DIR}/p5.h5)

#====
add_executable(shapes_check GetShape.f90)
target_link_libraries(shapes_check PRIVATE h5fortran)
target_include_directories(shapes_check PRIVATE ${CMAKE_BINARY_DIR})

add_test(NAME ShapeCheck COMMAND shapes_check ${testfile} /group69/flux_node)
set_tests_properties(ShapeCheck PROPERTIES
  FIXTURES_REQUIRED h5files
  REQUIRED_FILES ${testfile}
  TIMEOUT 15
  RUN_SERIAL true)

find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  add_test(NAME PythonShapeCheck
    COMMAND Python3::Interpreter GetShapes.py ${testfile} /group69/flux_node
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
  set_tests_properties(PythonShapeCheck PROPERTIES
    FIXTURES_REQUIRED h5files
    REQUIRED_FILES ${testfile}
    TIMEOUT 30
    RUN_SERIAL true)
endif(Python3_FOUND)
