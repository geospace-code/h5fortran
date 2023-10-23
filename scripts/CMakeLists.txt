cmake_minimum_required(VERSION 3.20)
project(HDF5_build
LANGUAGES C Fortran
)

option(hdf5_parallel "build HDF5 parallel MPI")
option(zlib_legacy "use legacy zlib 1.x")
option(CMAKE_TLS_VERIFY "verify TLS cert" on)

# --- system checks
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  message(FATAL_ERROR "please specify where to install HDF5 under, like
  cmake -B build -DCMAKE_INSTALL_PREFIX=~/hdf5")
endif()

if(hdf5_parallel)

if(NOT MPI_ROOT AND DEFINED ENV{MPI_ROOT})
  set(MPI_ROOT $ENV{MPI_ROOT})
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Linux" AND MPI_ROOT)
  set(ld_path $ENV{LD_LIBRARY_PATH})
  cmake_path(CONVERT "${ld_path}" TO_CMAKE_PATH_LIST ld_path NORMALIZE)
  cmake_path(CONVERT "${MPI_ROOT}" TO_CMAKE_PATH_LIST MPI_ROOT NORMALIZE)

  if(NOT "${ld_path}" MATCHES "${MPI_ROOT}/lib")
    message(WARNING "${MPI_ROOT}/lib not found in LD_LIBRARY_PATH: $ENV{LD_LIBRARY_PATH}
    HDF5 build may fail due to bugs in HDF5 package CMake scripts.
    Fix this by adding to ~/.bashrc or similar:
      export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${MPI_ROOT}/lib")
  endif()
endif()

endif()

# HDF5 install fails to work (link) if prior HDF5 library is installed there
find_library(_hdf5_libprior NAMES hdf5 PATHS ${CMAKE_INSTALL_PREFIX} PATH_SUFFIXES lib NO_DEFAULT_PATH NO_CACHE)
find_path(_hdf5_incprior NAMES hdf5.h PATHS ${CMAKE_INSTALL_PREFIX} PATH_SUFFIXES include NO_DEFAULT_PATH NO_CACHE)
find_program(_hdf5_binprior NAMES h5cc PATHS ${CMAKE_INSTALL_PREFIX} PATH_SUFFIXES bin NO_DEFAULT_PATH NO_CACHE)
if(_hdf5_binprior)
  cmake_path(GET _hdf5_binprior PARENT_PATH _hdf5_binprior)
else()
  set(_hdf5_binprior "")
endif()
if(_hdf5_libprior)
  cmake_path(GET _hdf5_libprior PARENT_PATH _hdf5_libprior)
endif()
if(_hdf5_libprior OR _hdf5_incprior OR _hdf5_binprior)
  message(FATAL_ERROR "HDF5 library already installed:
  ${_hdf5_libprior}
  ${_hdf5_incprior}
  ${_hdf5_binprior}
  Please pick a new install location or completely remove the old HDF5 install directory.
  Otherwise, HDF5 will fail to link correctly with prior version and this version mixed.")
endif()

# --- commence HDF5 build/install
set_property(DIRECTORY PROPERTY EP_UPDATE_DISCONNECTED true)

if(hdf5_parallel)
  find_package(MPI COMPONENTS C REQUIRED)
  include(${PROJECT_SOURCE_DIR}/../cmake/check_mpi.cmake)
  check_mpi_version()
endif()

include(${PROJECT_SOURCE_DIR}/../cmake/hdf5.cmake)

message(STATUS "Build / install HDF5 ${hdf5_tag} to ${CMAKE_INSTALL_PREFIX}")

# --- features
include(FeatureSummary)

add_feature_info(HDF5parallel hdf5_parallel "HDF5 MPI layer")

feature_summary(WHAT ENABLED_FEATURES DISABLED_FEATURES)
