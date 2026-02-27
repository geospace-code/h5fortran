# builds HDF5 library from scratch
# note: the use of "lib" vs. CMAKE_*_LIBRARY_PREFIX is deliberate based on HDF5
# across Intel Fortran on Windows (MSVC-like) vs. Gfortran on Windows vs. Linux.
include(FetchContent)

if(NOT DEFINED hdf5_req)
  set(hdf5_req 2.0)
endif()

file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json json)

# --- Zlib
# if(NOT zlib_url)
#   string(JSON zlib_url GET ${json} "zlib")
# endif()

# set(ZLIB_COMPAT on)
# set(BUILD_TESTING off)
# # set(CMAKE_POSITION_INDEPENDENT_CODE ON)
# # NetCDF 4.9/4.6 needs fPIC

# FetchContent_Declare(ZLIBdep
#   URL ${zlib_url}
#   FIND_PACKAGE_ARGS NAMES ZLIB
# )

# if(TARGET zlib-ng)
#   set(ZLIB_USE_EXTERNAL OFF)
#   set(ZLIB_ROOT ${zlib_BINARY_DIR})
# endif()

# --- HDF5
# https://forum.hdfgroup.org/t/issues-when-using-hdf5-as-a-git-submodule-and-using-cmake-with-add-subdirectory/7189/2

set(HDF5_ENABLE_Z_LIB_SUPPORT ON)
set(HDF5_ENABLE_ZLIB_SUPPORT ON)
set(HDF5_ALLOW_EXTERNAL_SUPPORT "TGZ")
set(HDF5_GENERATE_HEADERS false)
set(HDF5_DISABLE_COMPILER_WARNINGS true)
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

# -DHDF5_BUILD_PARALLEL_TOOLS:BOOL=false avoids error with HDF5 2.0 needing libMFU mpiFileUtils

# -DHDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16:BOOL=OFF avoids error with GCC or Clang
#  src/H5Tconv_integer.c:1746:75: error: 'FLT16_MAX' undeclared (first use in this function); did you mean 'INT16_MAX'?
#
# -DHDF5_USE_GNU_DIRS:BOOL=ON  # new for 1.14
# -DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=ON switched from -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON for HDF5 2.0

if(NOT hdf5_url)
  string(JSON hdf5_url GET ${json} "hdf5" "${hdf5_req}")
endif()

FetchContent_Declare(HDF5
  URL ${hdf5_url}
  FIND_PACKAGE_ARGS
    NAMES HDF5
    COMPONENTS HL Fortran
)

FetchContent_MakeAvailable(HDF5)# ZLIBdep)

if(EXISTS "${hdf5_SOURCE_DIR}/src/H5public.h")
  set(_h5public_h "${hdf5_SOURCE_DIR}/src/H5public.h")
elseif(EXISTS "${HDF5_C_INCLUDE_DIR}/H5public.h")
  set(_h5public_h "${HDF5_C_INCLUDE_DIR}/H5public.h")
else()
  message(FATAL_ERROR "Cannot find H5public.h for HDF5 version extraction
  ${HDF5_C_INCLUDE_DIR}")
endif()

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

message(STATUS "Building HDF5 version ${HDF5_VERSION}")

# --- imported target
if(NOT TARGET HDF5::HDF5)

function(print_targets dir)
    get_property(subdirs DIRECTORY "${dir}" PROPERTY SUBDIRECTORIES)
    foreach(sub IN LISTS subdirs)
        print_targets("${sub}")
    endforeach()

    get_directory_property(targets DIRECTORY "${dir}" BUILDSYSTEM_TARGETS)
    if(targets)
        message("Targets in ${dir}:")
        foreach(t IN LISTS targets)
            message("  • ${t}")
        endforeach()
    endif()
endfunction()

# print all targets from HDF5 for debugging
print_targets(${hdf5_SOURCE_DIR})

if(BUILD_SHARED_LIBS)
  set(_hdf5_lib_type "shared")
else()
  set(_hdf5_lib_type "static")
endif()

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/${_hdf5_lib_type})

add_library(HDF5::HDF5 INTERFACE IMPORTED)
target_link_libraries(HDF5::HDF5 INTERFACE
hdf5_hl_fortran-${_hdf5_lib_type}
hdf5_fortran-${_hdf5_lib_type}
hdf5_hl-${_hdf5_lib_type}
hdf5-${_hdf5_lib_type}
)
target_include_directories(HDF5::HDF5 INTERFACE ${h5fortran_BINARY_DIR}/include/${_hdf5_lib_type})

endif()

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

# --- external deps

# HDF5 bug #3663 for HDF5 1.14.2..2.0.0 at least
# https://github.com/HDFGroup/hdf5/issues/3663
# we have it here too so that built HDF5 library will link correctly with other project for scripts/ build
if(WIN32 AND CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
if(HDF5_VERSION VERSION_GREATER_EQUAL 1.14.2)
  message(STATUS "HDF5: applying workaround for HDFGroup/HDF5 bug #3663 with Intel oneAPI on Windows")
  target_link_libraries(HDF5::HDF5 INTERFACE shlwapi)
endif()
endif()
