cmake_minimum_required(VERSION 3.25...4.3)
# CMake 3.25 for find_*(VALIDATE)
# CMake >= 3.26 required if building HDF5

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "Please do out of source build like
  cmake -Bbuild")
endif()

project(h5fortran
LANGUAGES C Fortran
VERSION 4.13.0
)
# C language is only needed for FindHDF5 algorithms, not for the h5fortran project itself

include(CTest)

# to use our FindHDF5.cmake
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# --- avoid Anaconda libraries
if(h5fortran_python AND h5fortran_BUILD_TESTING)
  find_package(Python COMPONENTS Interpreter)
endif()

include(options.cmake)

if(DEFINED ENV{CONDA_PREFIX} AND h5fortran_IGNORE_CONDA_LIBRARIES)
  list(APPEND CMAKE_IGNORE_PREFIX_PATH $ENV{CONDA_PREFIX})
  list(APPEND CMAKE_IGNORE_PATH $ENV{CONDA_PREFIX}/bin $ENV{CONDA_PREFIX}/lib $ENV{CONDA_PREFIX}/include)
  # need CMAKE_IGNORE_PATH to ensure env var PATH doesn't interfere
  # despite CMAKE_IGNORE_PREFIX_PATH
endif()

# --- RPATH handling for transitive dependencies
if(h5fortran_ENABLE_RPATH AND NOT WIN32)
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_FULL_LIBDIR}")
  set(CMAKE_BUILD_RPATH "${CMAKE_INSTALL_FULL_LIBDIR}")
endif()

include(cmake/compilers.cmake)

set(CMAKE_Fortran_MODULE_DIRECTORY ${h5fortran_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR})
# HDF5 fetchcontent needs this at least for HDF5 2.0.0, 2.1.0, ...

# find or build HDF5
include(cmake/hdf5.cmake)

set(HDF5_VERSION ${HDF5_VERSION} CACHE STRING "HDF5 version")

if(h5fortran_IS_TOP_LEVEL AND (hdf5_parallel OR HDF5_IS_PARALLEL))
  target_link_libraries(HDF5::HDF5 INTERFACE MPI::MPI_Fortran)
endif()

# --- h5fortran library

add_library(h5fortran)
target_include_directories(h5fortran PUBLIC
$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
target_link_libraries(h5fortran PUBLIC HDF5::HDF5)
target_compile_options(h5fortran PRIVATE ${h5fortran_fflags})

file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY})
# avoid race condition when used by parent projects

set_property(TARGET h5fortran PROPERTY VERSION ${h5fortran_VERSION})

if(h5fortran_warning_as_error)
  set_property(TARGET h5fortran PROPERTY COMPILE_WARNING_AS_ERROR true)
  if(CMAKE_VERSION VERSION_GREATER_EQUAL 4.0)
    set_property(TARGET h5fortran PROPERTY LINK_WARNING_AS_ERROR true)
  endif()
endif()

add_library(h5fortran::h5fortran ALIAS h5fortran)


install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/h5fortran.mod TYPE INCLUDE)

install(TARGETS h5fortran EXPORT h5fortran-targets)

add_subdirectory(src)

get_property(_h5f_defs TARGET h5fortran PROPERTY COMPILE_DEFINITIONS)
message(VERBOSE "h5fortran COMPILE_DEFINITIONS: ${_h5f_defs}")
get_property(_h5f_flags TARGET h5fortran PROPERTY COMPILE_OPTIONS)
message(VERBOSE "h5fortran COMPILE_OPTIONS: ${_h5f_flags}")

message(VERBOSE "HDF5_INCLUDE_DIRS: ${HDF5_INCLUDE_DIRS}")
get_property(_h5f_libs TARGET h5fortran PROPERTY INCLUDE_DIRECTORIES)
message(VERBOSE "h5fortran INCLUDE_DIRECTORIES: ${_h5f_libs}")

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
