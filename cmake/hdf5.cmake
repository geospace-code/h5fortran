# builds HDF5 library from scratch
# note: the use of "lib" vs. CMAKE_*_LIBRARY_PREFIX is deliberate based on HDF5
# across Intel Fortran on Windows (MSVC-like) vs. Gfortran on Windows vs. Linux.
include(GNUInstallDirs)
include(FetchContent)
include(CheckSourceCompiles)


if(NOT DEFINED h5fortran_hdf5_req)
  set(h5fortran_hdf5_req "2.1patch")
endif()
# HDF5 2.x require CMake >= 3.26, but the benefits are so great that this is worthwhile

if(hdf5_parallel)
  set(HDF5_PREFER_PARALLEL ON)
endif()

file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json json)

# --- HDF5
# https://forum.hdfgroup.org/t/issues-when-using-hdf5-as-a-git-submodule-and-using-cmake-with-add-subdirectory/7189/2

set(ZLIB_USE_LOCALCONTENT OFF)

set(BUILD_STATIC_LIBS ON)

set(HDF5_GENERATE_HEADERS OFF)
set(HDF5_PACKAGE_EXTLIBS ON)
set(HDF5_DISABLE_COMPILER_WARNINGS ON)

set(ZLIB_VERSION "1.3.2")
set(ZLIB_GIT_TAG "v${ZLIB_VERSION}")
set(ZLIB_TGZ_NAME "zlib-${ZLIB_VERSION}.tar.gz")
set(ZLIB_TGZ_ORIGPATH "https://github.com/madler/zlib/archive/refs/tags/${ZLIB_GIT_TAG}")

if(h5fortran_hdf5_req STREQUAL "2.1patch" OR h5fortran_hdf5_req VERSION_GREATER_EQUAL "2.0")
  set(ZLIB_USE_EXTERNAL ON)
  set(HDF5_ALLOW_EXTERNAL_SUPPORT TGZ)
  if(h5fortran_hdf5_zlib)
    set(HDF5_ENABLE_ZLIB_SUPPORT ON)
  endif()

  # ZLIB_NG with HDF5 2.2 still has issues at build or link time with symbols.
  set(HDF5_USE_ZLIB_NG OFF)
  set(ZLIBNG_USE_EXTERNAL OFF)

# users need their own Zlib if using HDF5 < 2.x
elseif(h5fortran_hdf5_req MATCHES "^1\.(10|14)$")
  # HDF5 1.10 and 1.14 use HDF5_ENABLE_Z_LIB_SUPPORT
  # HDF5 2.x uses HDF5_ENABLE_ZLIB_SUPPORT
  set(HDF5_ENABLE_Z_LIB_SUPPORT ON CACHE BOOL "Enable ZLib support" FORCE)
  # HDF5 1.10 and 1.14 ZLIB fails to build Zlib despite trying
  # Error copying directory from "/" to "<build_dir>_deps/hdf5_zlib-src": Permission denied
  # set(ZLIB_USE_EXTERNAL ON CACHE BOOL "Use External Library Building for ZLIB else search" FORCE)
  # 1.14 also needs:
  # set(HDF5_ALLOW_EXTERNAL_SUPPORT "TGZ" CACHE STRING "Allow External Support for TGZ" FORCE)
endif()


set(HDF5_BUILD_FORTRAN ON CACHE BOOL "Build Fortran bindings" FORCE)
set(HDF5_BUILD_TOOLS ON CACHE BOOL "Build HDF5 tools" FORCE)
set(HDF5_BUILD_EXAMPLES OFF CACHE BOOL "Build HDF5 examples" FORCE)
set(HDF5_BUILD_HL_LIB ON CACHE BOOL "Build HDF5 High-Level library" FORCE)
set(HDF5_USE_GNU_DIRS ON CACHE BOOL "Use GNU install directories" FORCE)
set(HDF5_BUILD_CPP_LIB OFF CACHE BOOL "Build HDF5 C++ library" FORCE)

set(ZLIB_USE_LOCALCONTENT OFF CACHE BOOL "Use local file for ZLIB FetchContent" FORCE)
set(ZLIB_USE_LOCALCONTENT OFF)
# ---

set(BUILD_TESTING false)
set(HDF5_ENABLE_PARALLEL ${hdf5_parallel})
set(HDF5_BUILD_PARALLEL_TOOLS false)
set(HDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16 OFF)

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
  string(JSON hdf5_url GET ${json} "hdf5" "${h5fortran_hdf5_req}")
endif()


if(NOT TARGET HDF5::HDF5)

FetchContent_Declare(HDF5 URL ${hdf5_url} FIND_PACKAGE_ARGS COMPONENTS HL Fortran C)
# "C" as well so that link tests work and corner cases OK

FetchContent_MakeAvailable(HDF5)

endif()


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


macro(hdf5_imported_targets)

if(BUILD_SHARED_LIBS)
  set(_hdf5_lib_type "shared")
else()
  set(_hdf5_lib_type "static")
endif()
file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${_hdf5_lib_type})
# avoid race condition "Imported target "HDF5::HDF5" includes non-existent path"


add_library(HDF5::HDF5 INTERFACE IMPORTED)
# look under
# HDF5 2.x: ${h5fortran_BINARY_DIR}/_deps/hdf5-build/hdf5-targets.cmake
# HDF5 1.14: ${h5fortran_BINARY_DIR}/_deps/hdf5-build/hdf5-config.cmake look for hdf5_comp variable like hdf5_hl_fortran

target_link_libraries(HDF5::HDF5 INTERFACE
hdf5_hl_fortran-${_hdf5_lib_type}
hdf5_fortran-${_hdf5_lib_type}
hdf5_hl-${_hdf5_lib_type}
hdf5-${_hdf5_lib_type}
)

target_include_directories(HDF5::HDF5 INTERFACE ${CMAKE_Fortran_MODULE_DIRECTORY}/${_hdf5_lib_type})

if(h5fortran_hdf5_req STREQUAL "1.10")
  file(MAKE_DIRECTORY ${hdf5_BINARY_DIR}/mod/${_hdf5_lib_type})
  target_include_directories(HDF5::HDF5 INTERFACE ${hdf5_BINARY_DIR}/mod/${_hdf5_lib_type})
endif()

endmacro()


macro(windows_oneapi_hdf5_workaround)

# HDF5 bug #3663 for HDF5 1.14.2, 1.14.3, ...?
# https://github.com/HDFGroup/hdf5/issues/3663
if(WIN32 AND CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
if(HDF5_VERSION VERSION_GREATER_EQUAL 1.14.2 AND HDF5_VERSION VERSION_LESS 2.2.0)
  message(DEBUG "HDF5: applying workaround for HDFGroup/HDF5 bug #3663 with Intel oneAPI on Windows")
  list(APPEND CMAKE_REQUIRED_LIBRARIES shlwapi)
endif()
endif()

endmacro()


function(check_hdf5_compile lang)

set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5)
windows_oneapi_hdf5_workaround()

set(file "${CMAKE_CURRENT_FUNCTION_LIST_DIR}/check_hdf5")

if(lang STREQUAL "C")
  string(APPEND file ".c")
elseif(lang STREQUAL "Fortran")
  string(APPEND file ".f90")
endif()

file(READ "${file}" _src)
check_source_compiles(${lang} "${_src}" HDF5_${lang}_links)

endfunction()


function(check_hdf5 result_var)

check_hdf5_compile(C)
check_hdf5_compile(Fortran)

if(HDF5_C_links AND HDF5_Fortran_links)
  set(${result_var} true PARENT_SCOPE)
else()
  message(STATUS "HDF5 package failed compatibility validation with compiler ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
  set(${result_var} false PARENT_SCOPE)
endif()

endfunction()



if(NOT TARGET HDF5::HDF5)

  # we built HDF5, so define HDF5::HDF5 like FindHDF5.cmake find_package(HDF5)
  hdf5_imported_targets()

elseif(HDF5_FOUND)

  # the factory HDF5::HDF5 target FindHDF5 is missing HL_Fortran, so let's just define it.

  set_property(TARGET HDF5::HDF5 PROPERTY INTERFACE_LINK_LIBRARIES hdf5::hdf5_hl_fortran hdf5::hdf5_fortran hdf5::hdf5_hl hdf5::hdf5)

  check_hdf5(_hdf5_compatible)
  if(NOT _hdf5_compatible)
    message(FATAL_ERROR "HDF5 package found but incompatible with compiler ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}
Re-build with CMake option
  cmake -DFETCHCONTENT_TRY_FIND_PACKAGE_MODE=NEVER ...
")
  endif()

endif()
