cmake_minimum_required(VERSION 3.15)

project(HDF5_builder
  LANGUAGES C Fortran
  VERSION 1.12.0
  DESCRIPTION "download, build and install HDF5 library"
  HOMEPAGE_URL https://github.com/geospace-code/h5fortran)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  message(FATAL_ERROR "please set cmake -DCMAKE_INSTALL_PREFIX=~/lib or desired install location")
endif()

set(_hdf5_bindir ${CMAKE_INSTALL_PREFIX})

include(${CMAKE_CURRENT_SOURCE_DIR}/../cmake/build_hdf5.cmake)

add_custom_target(dummy DEPENDS HDF5proj)
