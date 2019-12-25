cmake_minimum_required (VERSION 3.12)  # 3.12 for Fortran submodule
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(hdf5iface
  LANGUAGES Fortran
  VERSION 2.0.0
  HOMEPAGE_URL https://github.com/scivision/h5fortran)
enable_testing()

include(cmake/compilers.cmake)

include(cmake/hdf5.cmake)

add_subdirectory(src)

add_subdirectory(tests)
