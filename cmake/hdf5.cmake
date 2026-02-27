# builds HDF5 library from scratch
# note: the use of "lib" vs. CMAKE_*_LIBRARY_PREFIX is deliberate based on HDF5
# across Intel Fortran on Windows (MSVC-like) vs. Gfortran on Windows vs. Linux.
include(GNUInstallDirs)
include(ExternalProject)
include(FetchContent)

if(NOT DEFINED hdf5_req)
  if(LINUX)
    set(hdf5_req 1.10)
  elseif(CMAKE_VERSION VERSION_GREATER_EQUAL 3.26)
    set(hdf5_req 2.0)
  else()
    set(hdf5_req 1.14)
  endif()
endif()

if(hdf5_parallel)
  find_package(MPI REQUIRED COMPONENTS C)
endif()

# pass MPI hints to HDF5
if(NOT MPI_ROOT AND DEFINED ENV{MPI_ROOT})
  set(MPI_ROOT $ENV{MPI_ROOT})
endif()

set(HDF5_LIBRARIES)
foreach(_name IN ITEMS hdf5_hl_fortran hdf5_hl_f90cstub hdf5_fortran hdf5_f90cstub hdf5_hl hdf5)
  if(BUILD_SHARED_LIBS)
    if(WIN32)
      list(APPEND HDF5_LIBRARIES ${CMAKE_INSTALL_FULL_BINDIR}/lib${_name}${CMAKE_SHARED_LIBRARY_SUFFIX})
    else()
      list(APPEND HDF5_LIBRARIES ${CMAKE_INSTALL_FULL_LIBDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${_name}${CMAKE_SHARED_LIBRARY_SUFFIX})
    endif()
  elseif(hdf5_req VERSION_GREATER_EQUAL 1.14)
    if(WIN32)
      list(APPEND HDF5_LIBRARIES ${CMAKE_INSTALL_FULL_LIBDIR}/lib${_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
    else()
      list(APPEND HDF5_LIBRARIES ${CMAKE_INSTALL_FULL_LIBDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}${_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
    endif()
  else()
    # need ${CMAKE_INSTALL_PREFIX}/lib as HDF5 didn't use GNUInstallDirs until HDF5 1.14
    list(APPEND HDF5_LIBRARIES ${CMAKE_INSTALL_PREFIX}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}${_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
  endif()
endforeach()

set(HDF5_INCLUDE_DIRS ${CMAKE_INSTALL_FULL_INCLUDEDIR})

if(BUILD_SHARED_LIBS)
  list(APPEND HDF5_INCLUDE_DIRS ${CMAKE_INSTALL_FULL_INCLUDEDIR}/mod/shared)
else()
  list(APPEND HDF5_INCLUDE_DIRS ${CMAKE_INSTALL_FULL_INCLUDEDIR}/mod/static)
endif()

file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json json)

# --- Zlib
set(zlib_dep)

if(TARGET ZLIB::ZLIB)
  add_custom_target(ZLIB)
elseif(h5fortran_find AND NOT build_zlib)
  find_package(ZLIB)
endif()

if(NOT TARGET ZLIB::ZLIB)
  set(zlib_dep DEPENDS ZLIB)
  include(${CMAKE_CURRENT_LIST_DIR}/zlib.cmake)
endif()

# by now, Zlib certainly exists as a target
if(NOT TARGET h5fortranZLIB::ZLIB)
  add_library(h5fortranZLIB::ZLIB ALIAS ZLIB::ZLIB)
endif()

# --- HDF5
# https://forum.hdfgroup.org/t/issues-when-using-hdf5-as-a-git-submodule-and-using-cmake-with-add-subdirectory/7189/2

set(hdf5_cmake_args
-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON
-DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=ON
-DZLIB_USE_EXTERNAL:BOOL=OFF
-DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}
-DCMAKE_MODULE_PATH:PATH=${CMAKE_MODULE_PATH}
-DHDF5_GENERATE_HEADERS:BOOL=false
-DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=true
-DBUILD_STATIC_LIBS:BOOL=$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
-DCMAKE_BUILD_TYPE=Release
-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
-DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
-DHDF5_BUILD_HL_LIB:BOOL=true
-DHDF5_BUILD_FORTRAN:BOOL=true
-DHDF5_BUILD_CPP_LIB:BOOL=false
-DBUILD_TESTING:BOOL=false
-DHDF5_BUILD_EXAMPLES:BOOL=false
-DHDF5_BUILD_TOOLS:BOOL=true
-DHDF5_ENABLE_PARALLEL:BOOL=$<BOOL:${hdf5_parallel}>
-DHDF5_BUILD_PARALLEL_TOOLS:BOOL=false
-DHDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16:BOOL=OFF
-DHDF5_USE_GNU_DIRS:BOOL=ON
)

# -DHDF5_BUILD_PARALLEL_TOOLS:BOOL=false avoids error with HDF5 2.0 needing libMFU mpiFileUtils

# -DHDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16:BOOL=OFF avoids error with GCC or Clang
#  src/H5Tconv_integer.c:1746:75: error: 'FLT16_MAX' undeclared (first use in this function); did you mean 'INT16_MAX'?
#
# -DHDF5_USE_GNU_DIRS:BOOL=ON  # new for 1.14
# -DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=ON switched from -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON for HDF5 2.0

if(MPI_ROOT)
  list(APPEND hdf5_cmake_args -DMPI_ROOT:PATH=${MPI_ROOT})
endif()

if(NOT hdf5_url)
  string(JSON hdf5_url GET ${json} "hdf5" "${hdf5_req}")
endif()

FetchContent_Populate(hdf5_upstream URL ${hdf5_url})

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

ExternalProject_Add(HDF5
SOURCE_DIR ${hdf5_upstream_SOURCE_DIR}
CMAKE_ARGS ${hdf5_cmake_args}
BUILD_BYPRODUCTS ${HDF5_LIBRARIES}
TEST_COMMAND ""
${zlib_dep}
CONFIGURE_HANDLED_BY_BUILD ON
USES_TERMINAL_CONFIGURE true
USES_TERMINAL_BUILD true
USES_TERMINAL_INSTALL true
)

# --- imported target

file(MAKE_DIRECTORY ${HDF5_INCLUDE_DIRS})
# avoid race condition

add_library(HDF5::HDF5 INTERFACE IMPORTED)
target_include_directories(HDF5::HDF5 INTERFACE "${HDF5_INCLUDE_DIRS}")
target_link_libraries(HDF5::HDF5 INTERFACE "${HDF5_LIBRARIES}")

add_dependencies(HDF5::HDF5 HDF5)

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
find_package(Threads)

target_link_libraries(HDF5::HDF5 INTERFACE
h5fortranZLIB::ZLIB
${CMAKE_THREAD_LIBS_INIT}
${CMAKE_DL_LIBS}
$<$<BOOL:${UNIX}>:m>
)

# HDF5 bug #3663 for HDF5 1.14.2..2.0.0 at least
# https://github.com/HDFGroup/hdf5/issues/3663
# we have it here too so that built HDF5 library will link correctly with other project for scripts/ build
if(WIN32 AND CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
if(HDF5_VERSION VERSION_GREATER_EQUAL 1.14.2)
  message(STATUS "HDF5: applying workaround for HDFGroup/HDF5 bug #3663 with Intel oneAPI on Windows")
  target_link_libraries(HDF5::HDF5 INTERFACE shlwapi)
endif()
endif()
