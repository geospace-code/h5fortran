cmake_minimum_required (VERSION 3.12)  # 3.12 for Fortran submodule
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(hdf5iface
  LANGUAGES Fortran
  VERSION 2.5.0
  DESCRIPTION "thin, light, easy HDF5 Fortran interface"
  HOMEPAGE_URL https://github.com/scivision/h5fortran)
enable_testing()

option(test_shaky "run shaky tests" OFF)

# list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

include(cmake/compilers.cmake)
include(cmake/hdf5.cmake)

set(CTEST_TEST_TIMEOUT 15)
# --- install config (before build)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  if(WIN32)
    set(HOME $ENV{USERPROFILE})
  else()
    set(HOME $ENV{HOME})
  endif()

  set(CMAKE_INSTALL_PREFIX "${HOME}/.local/h5fortran" CACHE PATH "..." FORCE)
endif()

# -- build

add_subdirectory(src)

add_subdirectory(src/tests)
