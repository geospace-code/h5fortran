cmake_minimum_required (VERSION 3.12)  # 3.12 for Fortran submodule
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(h5fortran
  LANGUAGES Fortran
  VERSION 2.5.3
  DESCRIPTION "thin, light, easy HDF5 Fortran interface"
  HOMEPAGE_URL https://github.com/scivision/h5fortran)
enable_testing()

option(test_shaky "run shaky tests" OFF)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
# set(HDF5_FIND_DEBUG true)

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/compilers.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/hdf5.cmake)

set(CTEST_TEST_TIMEOUT 15)

# --- build

add_subdirectory(src)

add_subdirectory(src/tests)

if(PROJECT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/install.cmake)
endif()