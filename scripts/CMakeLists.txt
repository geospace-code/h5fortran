cmake_minimum_required(VERSION 3.15)

if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(FATAL_ERROR "use cmake -B build or similar to avoid building in-source")
endif()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()

project(HDF5_builder
  LANGUAGES C Fortran
  VERSION 1.12.0
  DESCRIPTION "download, build and install HDF5 library"
  HOMEPAGE_URL https://github.com/geospace-code/h5fortran)

include(CTest)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  message(FATAL_ERROR "please set cmake -DCMAKE_INSTALL_PREFIX=~/lib or desired install location")
endif()

set(_hdf5_bindir ${CMAKE_INSTALL_PREFIX})

add_library(h5fortran INTERFACE) # dummy target

include(${CMAKE_CURRENT_SOURCE_DIR}/../cmake/build_hdf5.cmake)

if(BUILD_TESTING)

add_executable(test_minimal ${CMAKE_CURRENT_SOURCE_DIR}/../src/tests/test_minimal.f90)
target_link_libraries(test_minimal PRIVATE HDF5::HDF5 h5fortran)
add_test(NAME minimal COMMAND $<TARGET_FILE:test_minimal>)

endif(BUILD_TESTING)
