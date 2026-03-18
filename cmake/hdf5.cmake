# builds HDF5 library from scratch
# note: the use of "lib" vs. CMAKE_*_LIBRARY_PREFIX is deliberate based on HDF5
# across Intel Fortran on Windows (MSVC-like) vs. Gfortran on Windows vs. Linux.
include(GNUInstallDirs)
include(FetchContent)

if(NOT DEFINED hdf5_req)
  set(hdf5_req dev)
endif()
# HDF5 1.10.x can't build Zlib for itself.
# HDF5 2.0 and 2.1 require CMake >= 3.26, but the benefits are so great this is worthwhile

if(hdf5_parallel)
  find_package(MPI REQUIRED COMPONENTS C)
endif()

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
set(HDF5_ENABLE_PARALLEL ${hdf5_parallel})
set(HDF5_BUILD_PARALLEL_TOOLS false)
set(HDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16 OFF)
set(HDF5_USE_GNU_DIRS ON)

if(NOT DEFINED CMAKE_Fortran_MODULE_DIRECTORY)
  set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR})
endif()

# -DHDF5_BUILD_PARALLEL_TOOLS:BOOL=false avoids error with HDF5 2.0 needing libMFU mpiFileUtils

# -DHDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16:BOOL=OFF avoids error with GCC or Clang
#  src/H5Tconv_integer.c:1746:75: error: 'FLT16_MAX' undeclared (first use in this function); did you mean 'INT16_MAX'?
#
# -DHDF5_USE_GNU_DIRS:BOOL=ON  # new for 1.14
# -DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=ON switched from -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON for HDF5 2.0

if(NOT hdf5_url)
  string(JSON hdf5_url GET ${json} "hdf5" "${hdf5_req}")
endif()


set(_hdf5_fc_args)
if(h5fortran_find OR CRAY)
  if(CMAKE_VERSION VERSION_LESS 3.24)
    if(NOT TARGET HDF5::HDF5)
      find_package(HDF5 COMPONENTS HL Fortran)
    endif()
  else()
    set(_hdf5_fc_args FIND_PACKAGE_ARGS NAMES HDF5 COMPONENTS HL Fortran)
  endif()
endif()

if(NOT TARGET HDF5::HDF5)

FetchContent_Declare(HDF5
URL ${hdf5_url}
${_hdf5_fc_args}
)

FetchContent_MakeAvailable(HDF5)

if(NOT DEFINED HDF5_VERSION)

set(_h5public_h)
foreach(_hi IN ITEMS "${hdf5_SOURCE_DIR}/src/H5public.h" "${HDF5_DIR}/../../../include/H5public.h" "${HDF5_C_INCLUDE_DIR}/H5public.h")
  if(EXISTS "${_hi}")
    set(_h5public_h "${_hi}")
    message(DEBUG "Found H5public.h at ${_h5public_h}")
    break()
  endif()
endforeach()

if(_h5public_h)

# version extraction from HDF5 2.0 CMakeLists.txt
file (READ ${_h5public_h} _h5public_h_contents)
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MAJOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MAJOR ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MINOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MINOR ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_RELEASE[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_RELEASE ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_SUBRELEASE[ \t]+\"([0-9A-Za-z._-]*)\".*$"
    "\\1" H5_VERS_SUBRELEASE ${_h5public_h_contents})
set(HDF5_VERSION "${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")

message(STATUS "HDF5 version ${HDF5_VERSION}")

else()
  message(WARNING "Could not find H5public.h to determine HDF5 version")
endif()

endif()

# --- imported target

if(BUILD_SHARED_LIBS)
  set(_hdf5_lib_type "shared")
else()
  set(_hdf5_lib_type "static")
endif()


if(NOT TARGET HDF5::HDF5)
# this is defined by our cmake/FindHDF5.cmake find_package(HDF5)

add_library(HDF5::HDF5 INTERFACE IMPORTED)
target_link_libraries(HDF5::HDF5 INTERFACE
hdf5_hl_fortran-${_hdf5_lib_type}
hdf5_fortran-${_hdf5_lib_type}
hdf5_hl-${_hdf5_lib_type}
hdf5-${_hdf5_lib_type}
)

endif()


if(NOT HDF5_FOUND)

FetchContent_GetProperties(hdf5_zlib)
# FetchContent project hdf5_zlib is within HDFGroup/HDF5 project

if(NOT DEFINED hdf5_zlib_BINARY_DIR)
  message(FATAL_ERROR)
endif()

file(MAKE_DIRECTORY
  ${hdf5_zlib_BINARY_DIR}/mod/${_hdf5_lib_type}
  ${CMAKE_Fortran_MODULE_DIRECTORY}/${_hdf5_lib_type}
)
# avoid race condition "Imported target "HDF5::HDF5" includes non-existent path"

target_include_directories(HDF5::HDF5 INTERFACE
${hdf5_zlib_BINARY_DIR}/mod/${_hdf5_lib_type}
${CMAKE_Fortran_MODULE_DIRECTORY}/${_hdf5_lib_type}
)

endif()

endif()

if(h5fortran_IS_TOP_LEVEL AND HDF5_FOUND)
  check_hdf5()
endif()
