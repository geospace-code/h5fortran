cmake_minimum_required(VERSION 3.15...3.21)
project(HDF5_build LANGUAGES C Fortran)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  message(FATAL_ERROR "please specify where to install HDF5 under, like
  cmake -B build -DCMAKE_INSTALL_PREFIX=~/mylibs")
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/)

option(hdf5_parallel "build HDF5 parallel MPI (h5fortran doesn't use yet)")

include(${PROJECT_SOURCE_DIR}/../cmake/libraries.cmake)

set(EP_UPDATE_DISCONNECTED true)

message(STATUS "Building and installing HDF5 to ${CMAKE_INSTALL_PREFIX}")

include(${PROJECT_SOURCE_DIR}/../cmake/build_hdf5.cmake)

include(FeatureSummary)

add_feature_info(HDF5parallel hdf5_parallel "HDF5 MPI layer")

feature_summary(WHAT ALL)
