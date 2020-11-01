# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:

FindHDF5
---------

by Michael Hirsch www.scivision.dev

Finds HDF5 library for C, CXX, Fortran.


Result Variables
^^^^^^^^^^^^^^^^

``HDF5_FOUND``
  HDF5 libraries were found
``HDF5_INCLUDE_DIRS``
  HDF5 include directory
``HDF5_LIBRARIES``
  HDF5 library files

Components
==========

``C``

``CXX``

``Fortran``


Targets
^^^^^^^

``HDF5::HDF5``
  HDF5 Imported Target
#]=======================================================================]

set(_req)
set(_lsuf hdf5 hdf5/serial)
set(_psuf include include/static ${_lsuf})

# we don't use pkg-config directly because some distros pkg-config for HDF5 is broken
# however so far at least we've see the paths are often correct

find_package(PkgConfig)
if(PkgConfig_FOUND)
  pkg_search_module(pc_hdf5 hdf5 hdf5-serial)
  pkg_search_module(pc_zlib zlib)
endif()

if(Fortran IN_LIST HDF5_FIND_COMPONENTS)
  find_library(HDF5_Fortran_LIBRARY
    NAMES hdf5_fortran
    HINTS ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
    PATH_SUFFIXES ${_lsuf}
    NAMES_PER_DIR)
  find_library(HDF5_Fortran_HL_LIBRARY
    NAMES hdf5_hl_fortran hdf5hl_fortran
    HINTS ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
    PATH_SUFFIXES ${_lsuf}
    NAMES_PER_DIR)

  set(HDF5_Fortran_LIBRARIES ${HDF5_Fortran_HL_LIBRARY} ${HDF5_Fortran_LIBRARY})

  find_path(HDF5_Fortran_INCLUDE_DIR
    NAMES hdf5.mod
    HINTS ${pc_hdf5_INCLUDE_DIRS}
    PATH_SUFFIXES ${_psuf})

  list(APPEND _req ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_INCLUDE_DIR})
  if(HDF5_Fortran_HL_LIBRARY AND HDF5_Fortran_LIBRARY AND HDF5_Fortran_INCLUDE_DIR)
    set(HDF5_Fortran_FOUND true)
    set(HDF5_HL_FOUND true)
  endif()
endif()


if(CXX IN_LIST HDF5_FIND_COMPONENTS)
  find_library(HDF5_CXX_LIBRARY
    NAMES hdf5_cpp
    HINTS ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
    PATH_SUFFIXES ${_lsuf}
    NAMES_PER_DIR)
  find_library(HDF5_CXX_HL_LIBRARY
    NAMES hdf5_hl_cpp
    HINTS ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
    PATH_SUFFIXES ${_lsuf}
    NAMES_PER_DIR)

  set(HDF5_CXX_LIBRARIES ${HDF5_CXX_HL_LIBRARY} ${HDF5_CXX_LIBRARY})
  list(APPEND _req ${HDF5_CXX_LIBRARIES})
  if(HDF5_CXX_HL_LIBRARY AND HDF5_CXX_LIBRARY)
    set(HDF5_CXX_FOUND true)
    set(HDF5_HL_FOUND true)
  endif()
endif()

# C is always needed
find_library(HDF5_C_LIBRARY
  NAMES hdf5
  HINTS ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${_lsuf}
  NAMES_PER_DIR)
find_library(HDF5_C_HL_LIBRARY
  NAMES hdf5_hl
  HINTS ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${_lsuf}
  NAMES_PER_DIR)
set(HDF5_C_LIBRARIES ${HDF5_C_HL_LIBRARY} ${HDF5_C_LIBRARY})

find_path(HDF5_INCLUDE_DIR
  NAMES hdf5.h
  HINTS ${pc_hdf5_INCLUDE_DIRS}
  PATH_SUFFIXES ${_psuf})

list(APPEND _req ${HDF5_C_LIBRARIES} ${HDF5_INCLUDE_DIR})
if(HDF5_C_HL_LIBRARY AND HDF5_C_LIBRARY AND HDF5_INCLUDE_DIR)
  set(HDF5_C_FOUND true)
  set(HDF5_HL_FOUND true)
endif()

# required libraries
set(CMAKE_REQUIRED_INCLUDES ${HDF5_Fortran_INCLUDE_DIR} ${HDF5_INCLUDE_DIR})

if(HDF5_INCLUDE_DIR AND HDF5_C_FOUND)
include(CheckSymbolExists)
check_symbol_exists(H5_HAVE_FILTER_SZIP H5pubconf.h _szip)
check_symbol_exists(H5_HAVE_FILTER_DEFLATE H5pubconf.h _zlib)

# get version
# from CMake/Modules/FindHDF5.cmake
file(STRINGS ${HDF5_INCLUDE_DIR}/H5pubconf.h _def
REGEX "^[ \t]*#[ \t]*define[ \t]+H5_VERSION[ \t]+" )
if( "${_def}" MATCHES
"H5_VERSION[ \t]+\"([0-9]+\\.[0-9]+\\.[0-9]+)(-patch([0-9]+))?\"" )
  set(HDF5_VERSION "${CMAKE_MATCH_1}" )
  if( CMAKE_MATCH_3 )
    set(HDF5_VERSION ${HDF5_VERSION}.${CMAKE_MATCH_3})
  endif()
endif()

# otherwise can pickup miniconda zlib
get_filename_component(_hint ${HDF5_C_LIBRARY} DIRECTORY)
if(NOT ZLIB_ROOT)
  set(ZLIB_ROOT "${_hint}/..;${_hint}/../..;${pc_zlib_LIBRARY_DIRS};${pc_zlib_LIBDIR}")
endif()
if(NOT SZIP_ROOT)
  set(SZIP_ROOT "${ZLIB_ROOT}")
endif()

if(_zlib)
  find_package(ZLIB REQUIRED)

  if(_szip)
    # Szip even though not directly used because if system static links libhdf5 with szip,
    # our builds will fail if we don't also link szip.
    find_package(SZIP REQUIRED)
    set(CMAKE_REQUIRED_LIBRARIES ${HDF5_Fortran_LIBRARIES} ${HDF5_C_LIBRARIES} SZIP::SZIP ZLIB::ZLIB ${CMAKE_DL_LIBS})
  else()
    set(CMAKE_REQUIRED_LIBRARIES ${HDF5_Fortran_LIBRARIES} ${HDF5_C_LIBRARIES} ZLIB::ZLIB ${CMAKE_DL_LIBS})
  endif()
endif()

set(THREADS_PREFER_PTHREAD_FLAG true)
find_package(Threads)
if(Threads_FOUND)
  list(APPEND CMAKE_REQUIRED_LIBRARIES Threads::Threads)
endif()

# --- configure time checks
# these checks avoid messy, confusing errors at build time

if(HDF5_Fortran_FOUND)

include(CheckFortranSourceCompiles)
set(_code "program test_minimal
use hdf5, only : h5open_f, h5close_f
use h5lt, only : h5ltmake_dataset_f
implicit none (type, external)
integer :: i
call h5open_f(i)
if (i /= 0) error stop 'could not open hdf5 library'
call h5close_f(i)
if (i /= 0) error stop
end")
check_fortran_source_compiles(${_code} HDF5_links_ok SRC_EXT f90)

if(HDF5_links_ok)
  include(CheckFortranSourceRuns)
  check_fortran_source_runs(${_code} HDF5_runs_ok SRC_EXT f90)
endif(HDF5_links_ok)

elseif(HDF5_C_FOUND)

include(CheckCSourceCompiles)
set(_code "
#include \"hdf5.h\"

int main(void){
hid_t f = H5Fcreate (\"junk.h5\", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
status = H5Fclose (f);
return 0;
}")
check_c_source_compiles(${_code} HDF5_links_ok)
endif()

endif(HDF5_INCLUDE_DIR AND HDF5_C_FOUND)

set(CMAKE_REQUIRED_INCLUDES)
set(CMAKE_REQUIRED_LIBRARIES)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(HDF5
  REQUIRED_VARS _req HDF5_links_ok
  VERSION_VAR HDF5_VERSION
  HANDLE_COMPONENTS)

if(HDF5_FOUND)
  set(HDF5_INCLUDE_DIRS ${HDF5_Fortran_INCLUDE_DIR} ${HDF5_INCLUDE_DIR})
  set(HDF5_LIBRARIES ${HDF5_Fortran_LIBRARIES} ${HDF5_CXX_LIBRARIES} ${HDF5_C_LIBRARIES})

  if(NOT TARGET HDF5::HDF5)
    add_library(HDF5::HDF5 INTERFACE IMPORTED)
    set_target_properties(HDF5::HDF5 PROPERTIES
      INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
      INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}")
    if(_zlib)
      target_link_libraries(HDF5::HDF5 INTERFACE ZLIB::ZLIB)
    endif()
    if(_szip)
      target_link_libraries(HDF5::HDF5 INTERFACE SZIP::SZIP)
    endif()

    if(Threads_FOUND)
      target_link_libraries(HDF5::HDF5 INTERFACE Threads::Threads)
    endif()

    target_link_libraries(HDF5::HDF5 INTERFACE ${CMAKE_DL_LIBS})

    if(UNIX)
      target_link_libraries(HDF5::HDF5 INTERFACE m)
    endif()
  endif()
endif()

mark_as_advanced(HDF5_Fortran_LIBRARY HDF5_Fortran_HL_LIBRARY
HDF5_C_LIBRARY HDF5_C_HL_LIBRARY
HDF5_CXX_LIBRARY HDF5_CXX_HL_LIBRARY
HDF5_INCLUDE_DIR HDF5_Fortran_INCLUDE_DIR)
