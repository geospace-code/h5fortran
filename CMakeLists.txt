cmake_minimum_required(VERSION 3.20...3.30)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "Please do out of source build like
  cmake -Bbuild")
endif()

project(h5fortran
LANGUAGES C Fortran
VERSION 4.10.6
)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

if(NOT DEFINED CRAY AND DEFINED ENV{PE_ENV})
  set(CRAY true)
endif()

# --- avoid Anaconda libraries
find_package(Python COMPONENTS Interpreter)

if(DEFINED ENV{CONDA_PREFIX})
  list(APPEND CMAKE_IGNORE_PREFIX_PATH $ENV{CONDA_PREFIX})
  list(APPEND CMAKE_IGNORE_PATH $ENV{CONDA_PREFIX}/bin)
  # need CMAKE_IGNORE_PATH for CMake < 3.23
  # and to ensure system env var PATH doesn't interfere
  # despite CMAKE_IGNORE_PREFIX_PATH
endif()

include(options.cmake)
include(cmake/compilers.cmake)
include(cmake/CheckHDF5.cmake)

if(NOT TARGET HDF5::HDF5 AND (h5fortran_find OR CRAY))
  find_package(HDF5 COMPONENTS Fortran)
endif()

if(h5fortran_IS_TOP_LEVEL AND HDF5_FOUND)
  check_hdf5()
elseif(NOT TARGET HDF5::HDF5)
  include(cmake/hdf5.cmake)
endif()

set(HDF5_VERSION ${HDF5_VERSION} CACHE STRING "HDF5 version")

if(h5fortran_IS_TOP_LEVEL AND (hdf5_parallel OR HDF5_HAVE_PARALLEL))
  target_link_libraries(HDF5::HDF5 INTERFACE MPI::MPI_Fortran)
endif()

# HDF5 bug #3663 for HDF5 1.14.2..5, ...?
# https://github.com/HDFGroup/hdf5/issues/3663
if(WIN32 AND CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
if(HDF5_VERSION MATCHES "1.14.[2-5]")
  message(STATUS "HDF5: applying workaround for HDF5 bug #3663 with Intel oneAPI on Windows")
  target_link_libraries(HDF5::HDF5 INTERFACE shlwapi)
endif()
endif()

# --- h5fortran library

add_library(h5fortran)
target_include_directories(h5fortran PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>
)
target_link_libraries(h5fortran PUBLIC HDF5::HDF5)
set_property(TARGET h5fortran PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET h5fortran PROPERTY VERSION ${PROJECT_VERSION})

# GLOBAL needed for use by parent projects
add_library(h5fortran::h5fortran INTERFACE IMPORTED GLOBAL)
target_link_libraries(h5fortran::h5fortran INTERFACE h5fortran)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
# avoid race condition when used by parent projects

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

install(TARGETS h5fortran EXPORT h5fortran-targets)

add_subdirectory(src)

get_property(_h5f_defs TARGET h5fortran PROPERTY COMPILE_DEFINITIONS)
message(VERBOSE "h5fortran COMPILE_DEFINITIONS: ${_h5f_defs}")
get_property(_h5f_flags TARGET h5fortran PROPERTY COMPILE_OPTIONS)
message(VERBOSE "h5fortran COMPILE_OPTIONS: ${_h5f_flags}")
get_property(_h5f_libs TARGET h5fortran PROPERTY LINK_LIBRARIES)
message(VERBOSE "h5fortran LINK_LIBRARIES: ${_h5f_libs}")


if(h5fortran_BUILD_TESTING)
  add_subdirectory(test)
endif()

# additional Find*.cmake necessary
install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindHDF5.cmake
DESTINATION cmake
)

include(cmake/pkgconf.cmake)
include(cmake/install.cmake)

file(GENERATE OUTPUT .gitignore CONTENT "*")
