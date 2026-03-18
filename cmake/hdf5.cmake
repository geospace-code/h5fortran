# builds HDF5 library from scratch
# note: the use of "lib" vs. CMAKE_*_LIBRARY_PREFIX is deliberate based on HDF5
# across Intel Fortran on Windows (MSVC-like) vs. Gfortran on Windows vs. Linux.
include(GNUInstallDirs)
include(ExternalProject)
include(FetchContent)

if(NOT DEFINED hdf5_req)
  set(hdf5_req dev)
endif()
# HDF5 1.10.x can't build Zlib for itself.
# HDF5 2.0 and 2.1 require CMake >= 3.26, but the benefits are so great this is worthwhile

if(hdf5_parallel)
  find_package(MPI REQUIRED COMPONENTS C)
endif()

# pass MPI hints to HDF5
# if(NOT MPI_ROOT AND DEFINED ENV{MPI_ROOT})
#   set(MPI_ROOT $ENV{MPI_ROOT})
# endif()

file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json json)

# --- HDF5
# https://forum.hdfgroup.org/t/issues-when-using-hdf5-as-a-git-submodule-and-using-cmake-with-add-subdirectory/7189/2

set(HDF5_ENABLE_ZLIB_SUPPORT ON)
set(HDF5_PACKAGE_EXTLIBS ON)
set(HDF5_USE_ZLIB_NG OFF)
set(ZLIB_USE_EXTERNAL ON)
set(ZLIBNG_USE_EXTERNAL OFF)
set(HDF5_ALLOW_EXTERNAL_SUPPORT TGZ)
set(ZLIB_USE_LOCALCONTENT OFF)
set(HDF5_GENERATE_HEADERS false)
set(HDF5_DISABLE_COMPILER_WARNINGS true)
set(BUILD_STATIC_LIBS ON)
set(CMAKE_BUILD_TYPE Release)
set(HDF5_BUILD_HL_LIB true)
set(HDF5_BUILD_FORTRAN true)
set(HDF5_BUILD_CPP_LIB false)
set(BUILD_TESTING false)
set(HDF5_BUILD_EXAMPLES false)
set(HDF5_BUILD_TOOLS true)
set(HDF5_ENABLE_PARALLEL false)
set(HDF5_BUILD_PARALLEL_TOOLS false)
set(HDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16 OFF)
set(HDF5_USE_GNU_DIRS ON)

set(CMAKE_Fortran_MODULE_DIRECTORY ${h5fortran_BINARY_DIR}/include)

# -DHDF5_BUILD_PARALLEL_TOOLS:BOOL=false avoids error with HDF5 2.0 needing libMFU mpiFileUtils

# -DHDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16:BOOL=OFF avoids error with GCC or Clang
#  src/H5Tconv_integer.c:1746:75: error: 'FLT16_MAX' undeclared (first use in this function); did you mean 'INT16_MAX'?
#
# -DHDF5_USE_GNU_DIRS:BOOL=ON  # new for 1.14
# -DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=ON switched from -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON for HDF5 2.0

# if(MPI_ROOT)
#   list(APPEND hdf5_cmake_args -DMPI_ROOT:PATH=${MPI_ROOT})
# endif()

if(NOT hdf5_url)
  string(JSON hdf5_url GET ${json} "hdf5" "${hdf5_req}")
endif()

FetchContent_Declare(hdf5_upstream URL ${hdf5_url})
FetchContent_MakeAvailable(hdf5_upstream)

# version extraction from HDF5 2.0 CMakeLists.txt
file (READ ${hdf5_upstream_SOURCE_DIR}/src/H5public.h _h5public_h_contents)
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MAJOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MAJOR ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MINOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MINOR ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_RELEASE[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_RELEASE ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_SUBRELEASE[ \t]+\"([0-9A-Za-z._-]*)\".*$"
    "\\1" H5_VERS_SUBRELEASE ${_h5public_h_contents})
set(HDF5_VERSION "${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")

message(STATUS "Building HDF5 version ${HDF5_VERSION}")

# --- imported target


FetchContent_GetProperties(hdf5_zlib)

if(NOT DEFINED hdf5_zlib_BINARY_DIR)
  message(FATAL_ERROR)
endif()

file(MAKE_DIRECTORY ${hdf5_zlib_BINARY_DIR}/mod/static ${h5fortran_BINARY_DIR}/include/static)
# avoid race condition "Imported target "HDF5::HDF5" includes non-existent path"

add_library(HDF5::HDF5 INTERFACE IMPORTED)
target_link_libraries(HDF5::HDF5 INTERFACE
hdf5_hl_fortran-static
hdf5_fortran-static
hdf5_hl-static
hdf5-static
)
target_include_directories(HDF5::HDF5 INTERFACE
${hdf5_zlib_BINARY_DIR}/mod/static
${h5fortran_BINARY_DIR}/include/static
)

# --- HDF5 parallel compression support
# this could be improved by making it an ExternalProject post-build step instead of assumptions made here
if(hdf5_parallel)
  if(MPI_VERSION VERSION_GREATER_EQUAL 3)
    message(STATUS "Building HDF5-MPI: MPI-3 available, assuming HDF5 parallel compression enabled")
    set(hdf5_parallel_compression ".true." CACHE STRING "configure variable for HDF5 parallel compression")
  else()
    message(STATUS "Building HDF5-MPI: MPI-3 NOT available => HDF5 parallel compression disabled")
    set(hdf5_parallel_compression ".false." CACHE STRING "configure variable for HDF5 parallel compression: MPI < 3")
  endif()
endif()

# HDF5 bug #3663 for HDF5 1.14.2..2.0.0 at least
# https://github.com/HDFGroup/hdf5/issues/3663
# we have it here too so that built HDF5 library will link correctly with other project for scripts/ build
if(WIN32 AND CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
if(HDF5_VERSION VERSION_GREATER_EQUAL 1.14.2)
  message(STATUS "HDF5: applying workaround for HDFGroup/HDF5 bug #3663 with Intel oneAPI on Windows")
  target_link_libraries(HDF5::HDF5 INTERFACE shlwapi)
endif()
endif()
