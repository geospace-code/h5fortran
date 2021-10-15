cmake_minimum_required(VERSION 3.20...3.22)
project(HDF5_build
LANGUAGES C Fortran
)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  message(FATAL_ERROR "please specify where to install HDF5 under, like
  cmake -B build -DCMAKE_INSTALL_PREFIX=~/mylibs")
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/)

option(hdf5_parallel "build HDF5 parallel MPI")

if(NOT MPI_ROOT AND DEFINED ENV{MPI_ROOT})
  set(MPI_ROOT $ENV{MPI_ROOT})
endif()

if(CMAKE_SYSTEM_NAME STREQUAL Linux AND MPI_ROOT)
  set(ld_path $ENV{LD_LIBRARY_PATH})
  cmake_path(CONVERT "${ld_path}" TO_CMAKE_PATH_LIST ld_path NORMALIZE)
  cmake_path(CONVERT "${MPI_ROOT}" TO_CMAKE_PATH_LIST MPI_ROOT NORMALIZE)

  if(NOT "${ld_path}" MATCHES "${MPI_ROOT}/lib")
    message(STATUS "${MPI_ROOT}/lib not found in LD_LIBRARY_PATH: $ENV{LD_LIBRARY_PATH}
    HDF5 build may fail due to bugs in their CMake scripts.
    Fix this by adding to ~/.bashrc or similar:
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${MPI_ROOT}")
  endif()
endif()

include(${PROJECT_SOURCE_DIR}/../cmake/libraries.cmake)

set(EP_UPDATE_DISCONNECTED true)

message(STATUS "Building and installing HDF5 to ${CMAKE_INSTALL_PREFIX}")

include(${PROJECT_SOURCE_DIR}/../cmake/build_hdf5.cmake)

include(FeatureSummary)

add_feature_info(HDF5parallel hdf5_parallel "HDF5 MPI layer")

feature_summary(WHAT ALL)
