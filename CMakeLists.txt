cmake_minimum_required (VERSION 3.12)  # 3.12 for Fortran submodule
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(hdf5iface Fortran)
enable_testing()

include(cmake/compilers.cmake)

include(cmake/hdf5.cmake)

add_subdirectory(src)

add_subdirectory(tests)

add_executable(shapes_check src/GetShape.f90)
target_link_libraries(shapes_check PRIVATE hdf5oo)
add_test(NAME ShapeCheck COMMAND shapes_check p5.h5 group69/flux_node)
set_tests_properties(ShapeCheck PROPERTIES FIXTURES_REQUIRED h5files)
