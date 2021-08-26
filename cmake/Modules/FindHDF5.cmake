# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:

FindHDF5
---------

by Michael Hirsch www.scivision.dev

Finds HDF5 library for C, CXX, Fortran. Serial or parallel HDF5.


Result Variables
^^^^^^^^^^^^^^^^

``HDF5_FOUND``
  HDF5 libraries were found

``HDF5_INCLUDE_DIRS``
  HDF5 include directory

``HDF5_LIBRARIES``
  HDF5 library files

``HDF5_<lang>_COMPILER_EXECUTABLE``
  wrapper compiler for HDF5

``HDF5_HAVE_PARALLEL``
  HDF5 links the MPI library (thus user program must link MPI as well)

Components
==========

``C``
  C is normally available for all HDF5 library installs

``CXX``
  C++ is an optional feature that not all HDF5 library installs are built with

``Fortran``
  Fortran is an optional feature that not all HDF5 library installs are built with

``parallel``
  checks that the optional MPI parallel HDF5 layer is enabled. NOTE: if HDF5_parallel_FOUND is true,
  the user program MUST link MPI::MPI_C and/or MPI::MPI_Fortran.

``HL``
  always implied and silently accepted to keep compatibility with factory FindHDF5.cmake


Targets
^^^^^^^

``HDF5::HDF5``
  HDF5 Imported Target
#]=======================================================================]

include(CheckSymbolExists)
include(CheckSourceCompiles)

function(get_flags exec outvar)

execute_process(COMMAND ${exec} -show
OUTPUT_STRIP_TRAILING_WHITESPACE
OUTPUT_VARIABLE ret
RESULT_VARIABLE code
TIMEOUT 10
)

if(code EQUAL 0)
  set(${outvar} ${ret} PARENT_SCOPE)
endif()

endfunction(get_flags)


function(pop_flag raw flag outvar)
# this gives the argument to flags to get their paths like -I or -l or -L

set(_v)
string(REGEX MATCHALL "(^| )${flag} *([^\" ]+|\"[^\"]+\")" _vars "${raw}")
foreach(_p IN LISTS _vars)
  string(REGEX REPLACE "(^| )${flag} *" "" _p "${_p}")
  list(APPEND _v "${_p}")
endforeach()

set(${outvar} ${_v} PARENT_SCOPE)

endfunction(pop_flag)


function(detect_config)

if(Fortran IN_LIST HDF5_FIND_COMPONENTS AND NOT HDF5_Fortran_FOUND)
  return()
endif()

if(CXX IN_LIST HDF5_FIND_COMPONENTS AND NOT HDF5_CXX_FOUND)
  return()
endif()

set(CMAKE_REQUIRED_INCLUDES ${HDF5_C_INCLUDE_DIR})

find_file(h5_conf
  NAMES H5pubconf.h H5pubconf-64.h
  HINTS ${HDF5_C_INCLUDE_DIR}
  NO_DEFAULT_PATH
)

if(NOT h5_conf)
  set(HDF5_C_FOUND false PARENT_SCOPE)
  return()
endif()

# check HDF5 features that require link of external libraries.
check_symbol_exists(H5_HAVE_FILTER_SZIP ${h5_conf} hdf5_have_szip)
check_symbol_exists(H5_HAVE_FILTER_DEFLATE ${h5_conf} hdf5_have_zlib)

# Always check for HDF5 MPI support because HDF5 link fails if MPI is linked into HDF5.
check_symbol_exists(H5_HAVE_PARALLEL ${h5_conf} HDF5_HAVE_PARALLEL)

set(HDF5_parallel_FOUND false PARENT_SCOPE)

if(HDF5_HAVE_PARALLEL)
  set(mpi_comp C)
  if(Fortran IN_LIST HDF5_FIND_COMPONENTS)
    list(APPEND mpi_comp Fortran)
  endif()
  find_package(MPI COMPONENTS ${mpi_comp})

  if(MPI_FOUND)
    set(HDF5_parallel_FOUND true PARENT_SCOPE)
  endif()
endif()

# get version
# from CMake/Modules/FindHDF5.cmake
file(STRINGS ${h5_conf} _def
REGEX "^[ \t]*#[ \t]*define[ \t]+H5_VERSION[ \t]+" )
if("${_def}" MATCHES
"H5_VERSION[ \t]+\"([0-9]+\\.[0-9]+\\.[0-9]+)(-patch([0-9]+))?\"" )
  set(HDF5_VERSION "${CMAKE_MATCH_1}" )
  if(CMAKE_MATCH_3)
    set(HDF5_VERSION ${HDF5_VERSION}.${CMAKE_MATCH_3})
  endif()

  set(HDF5_VERSION ${HDF5_VERSION} PARENT_SCOPE)
endif()

# avoid picking up incompatible zlib over the desired zlib
cmake_path(GET HDF5_C_INCLUDE_DIR PARENT_PATH zlib_dir)
if(NOT ZLIB_ROOT)
  set(ZLIB_ROOT "${HDF5_ROOT};${zlib_dir}")
endif()
if(NOT SZIP_ROOT)
  set(SZIP_ROOT "${ZLIB_ROOT}")
endif()

if(hdf5_have_zlib)
  find_package(ZLIB)

  if(hdf5_have_szip)
    # Szip even though not used by default.
    # If system HDF5 dynamically links libhdf5 with szip, our builds will fail if we don't also link szip.
    # however, we don't require SZIP for this case as other HDF5 libraries may statically link SZIP.
    find_package(SZIP)
    list(APPEND CMAKE_REQUIRED_INCLUDES ${SZIP_INCLUDE_DIRS})
    list(APPEND CMAKE_REQUIRED_LIBRARIES ${SZIP_LIBRARIES})
  endif()

  list(APPEND CMAKE_REQUIRED_INCLUDES ${ZLIB_INCLUDE_DIRS})
  list(APPEND CMAKE_REQUIRED_LIBRARIES ${ZLIB_LIBRARIES})
endif()

list(APPEND CMAKE_REQUIRED_LIBRARIES ${CMAKE_DL_LIBS})

find_package(Threads)
list(APPEND CMAKE_REQUIRED_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})

if(UNIX)
  list(APPEND CMAKE_REQUIRED_LIBRARIES m)
endif()

set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} PARENT_SCOPE)

endfunction(detect_config)


function(find_hdf5_fortran)
# NOTE: the "lib*" are for Windows Intel compiler, even for self-built HDF5.
# CMake won't look for lib prefix automatically.

hdf5_fortran_wrap(hdf5_lib_dirs hdf5_inc_dirs)

set(_names hdf5_fortran)
set(_hl_names hdf5_hl_fortran hdf5hl_fortran)
set(_hl_stub_names hdf5_hl_f90cstub)
set(_stub_names hdf5_f90cstub)
if(MSVC)
  list(APPEND _names libhdf5_fortran)
  list(APPEND _hl_names libhdf5_hl_fortran libhdf5hl_fortran)
  list(APPEND _hl_stub_names libhdf5_hl_f90cstub)
  list(APPEND _stub_names libhdf5_f90cstub)
endif()

find_library(HDF5_Fortran_LIBRARY
  NAMES ${_names}
  HINTS ${hdf5_lib_dirs} ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${hdf5_lsuf}
  NAMES_PER_DIR
  DOC "HDF5 Fortran API")

find_library(HDF5_Fortran_HL_LIBRARY
  NAMES ${_hl_names}
  HINTS ${hdf5_lib_dirs} ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${hdf5_lsuf}
  NAMES_PER_DIR
  DOC "HDF5 Fortran HL high-level API")

# not all platforms have this stub
find_library(HDF5_Fortran_HL_stub
  NAMES ${_hl_stub_names}
  HINTS ${hdf5_lib_dirs} ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${hdf5_lsuf}
  NAMES_PER_DIR
  DOC "Fortran C HL interface, not all HDF5 implementations have/need this")

find_library(HDF5_Fortran_stub
  NAMES ${_stub_names}
  HINTS ${hdf5_lib_dirs} ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${hdf5_lsuf}
  NAMES_PER_DIR
  DOC "Fortran C interface, not all HDF5 implementations have/need this")

set(HDF5_Fortran_LIBRARIES ${HDF5_Fortran_HL_LIBRARY} ${HDF5_Fortran_LIBRARY})
if(HDF5_Fortran_HL_stub AND HDF5_Fortran_stub)
  list(APPEND HDF5_Fortran_LIBRARIES ${HDF5_Fortran_HL_stub} ${HDF5_Fortran_stub})
endif()

find_path(HDF5_Fortran_INCLUDE_DIR
  NAMES hdf5.mod
  HINTS ${hdf5_inc_dirs} ${pc_hdf5_INCLUDE_DIRS}
  PATH_SUFFIXES ${hdf5_msuf}
  PATHS ${hdf5_binpref}
  DOC "HDF5 Fortran modules")

if(HDF5_Fortran_LIBRARY AND HDF5_Fortran_HL_LIBRARY AND HDF5_Fortran_INCLUDE_DIR)
  set(HDF5_Fortran_LIBRARIES ${HDF5_Fortran_LIBRARIES} PARENT_SCOPE)
  set(HDF5_Fortran_INCLUDE_DIR ${HDF5_Fortran_INCLUDE_DIR} PARENT_SCOPE)
  set(HDF5_Fortran_FOUND true PARENT_SCOPE)
  set(HDF5_HL_FOUND true PARENT_SCOPE)
endif()

endfunction(find_hdf5_fortran)


function(find_hdf5_cxx)

hdf5_cxx_wrap(hdf5_lib_dirs hdf5_inc_dirs)

set(_names hdf5_cpp)
set(_hl_names hdf5_hl_cpp)
if(MSVC)
  list(APPEND _names libhdf5_cpp)
  list(APPEND _hl_names libhdf5_hl_cpp)
endif()

find_library(HDF5_CXX_LIBRARY
  NAMES ${_names}
  HINTS ${hdf5_lib_dirs} ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${hdf5_lsuf}
  NAMES_PER_DIR
  DOC "HDF5 C++ API")

find_library(HDF5_CXX_HL_LIBRARY
  NAMES ${_hl_names}
  HINTS ${hdf5_lib_dirs} ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${hdf5_lsuf}
  NAMES_PER_DIR
  DOC "HDF5 C++ high-level API")

find_path(HDF5_CXX_INCLUDE_DIR
  NAMES hdf5.h
  HINTS ${hdf5_inc_dirs} ${pc_hdf5_INCLUDE_DIRS}
  PATH_SUFFIXES ${_psuf}
  DOC "HDF5 C header")

if(HDF5_CXX_LIBRARY AND HDF5_CXX_HL_LIBRARY AND HDF5_CXX_INCLUDE_DIR)
  set(HDF5_CXX_LIBRARIES ${HDF5_CXX_HL_LIBRARY} ${HDF5_CXX_LIBRARY} PARENT_SCOPE)
  set(HDF5_CXX_INCLUDE_DIR ${HDF5_CXX_INCLUDE_DIR} PARENT_SCOPE)
  set(HDF5_CXX_FOUND true PARENT_SCOPE)
  set(HDF5_HL_FOUND true PARENT_SCOPE)
endif()

endfunction(find_hdf5_cxx)


function(find_hdf5_c)

hdf5_c_wrap(hdf5_lib_dirs hdf5_inc_dirs)

set(_names hdf5)
set(_hl_names hdf5_hl)

if(MSVC)
  list(APPEND _names libhdf5)
  list(APPEND _hl_names libhdf5_hl)
endif()

find_library(HDF5_C_LIBRARY
  NAMES ${_names}
  HINTS ${hdf5_lib_dirs} ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${hdf5_lsuf}
  NAMES_PER_DIR
  DOC "HDF5 C library (necessary for all languages)")

find_library(HDF5_C_HL_LIBRARY
  NAMES ${_hl_names}
  HINTS ${hdf5_lib_dirs} ${pc_hdf5_LIBRARY_DIRS} ${pc_hdf5_LIBDIR}
  PATH_SUFFIXES ${hdf5_lsuf}
  NAMES_PER_DIR
  DOC "HDF5 C high level interface")

find_path(HDF5_C_INCLUDE_DIR
  NAMES hdf5.h
  HINTS ${hdf5_inc_dirs} ${pc_hdf5_INCLUDE_DIRS}
  PATH_SUFFIXES ${_psuf}
  DOC "HDF5 C header")

if(HDF5_C_HL_LIBRARY AND HDF5_C_LIBRARY AND HDF5_C_INCLUDE_DIR)
  set(HDF5_C_LIBRARIES ${HDF5_C_HL_LIBRARY} ${HDF5_C_LIBRARY} PARENT_SCOPE)
  set(HDF5_C_INCLUDE_DIR ${HDF5_C_INCLUDE_DIR} PARENT_SCOPE)
  set(HDF5_C_FOUND true PARENT_SCOPE)
  set(HDF5_HL_FOUND true PARENT_SCOPE)
endif()

endfunction(find_hdf5_c)


function(hdf5_fortran_wrap lib_var inc_var)

set(lib_dirs)
set(inc_dirs)

set(wrapper_names h5pfc)
if(NOT parallel IN_LIST HDF5_FIND_COMPONENTS)
  list(PREPEND wrapper_names h5fc h5fc-64)
endif()

find_program(HDF5_Fortran_COMPILER_EXECUTABLE
  NAMES ${wrapper_names}
  HINTS ENV HOMEBREW_PREFIX
  PATHS ${hdf5_binpref}
  PATH_SUFFIXES ${hdf5_binsuf}
)

if(HDF5_Fortran_COMPILER_EXECUTABLE)
  get_flags(${HDF5_Fortran_COMPILER_EXECUTABLE} f_raw)
  if(f_raw)
    pop_flag(${f_raw} -L lib_dirs)
    pop_flag(${f_raw} -I inc_dirs)
    if(NOT inc_dirs AND parallel IN_LIST HDF5_FIND_COMPONENTS)
      get_flags(${MPI_Fortran_COMPILER} f_raw)
      if(f_raw)
        pop_flag(${f_raw} -I inc_dirs)
      endif(f_raw)
    endif()
  endif(f_raw)
endif()

if(inc_dirs)
  set(${inc_var} ${inc_dirs} PARENT_SCOPE)
endif()

if(lib_dirs)
  set(${lib_var} ${lib_dirs} PARENT_SCOPE)
endif()

endfunction(hdf5_fortran_wrap)


function(hdf5_cxx_wrap lib_var inc_var)

set(lib_dirs)
set(inc_dirs)

find_program(HDF5_CXX_COMPILER_EXECUTABLE
  NAMES h5c++ h5c++-64
  HINTS ENV HOMEBREW_PREFIX
  PATHS ${hdf5_binpref}
  PATH_SUFFIXES ${hdf5_binsuf}
)

if(HDF5_CXX_COMPILER_EXECUTABLE)
  get_flags(${HDF5_CXX_COMPILER_EXECUTABLE} cxx_raw)
  if(cxx_raw)
    pop_flag(${cxx_raw} -L lib_dirs)
    pop_flag(${cxx_raw} -I inc_dirs)
  endif(cxx_raw)
endif()

if(inc_dirs)
  set(${inc_var} ${inc_dirs} PARENT_SCOPE)
endif()

if(lib_dirs)
  set(${lib_var} ${lib_dirs} PARENT_SCOPE)
endif()

endfunction(hdf5_cxx_wrap)


function(hdf5_c_wrap lib_var inc_var)

set(lib_dirs)
set(inc_dirs)

set(wrapper_names h5pcc)
if(NOT parallel IN_LIST HDF5_FIND_COMPONENTS)
  list(PREPEND wrapper_names h5cc h5cc-64)
endif()

find_program(HDF5_C_COMPILER_EXECUTABLE
  NAMES ${wrapper_names}
  HINTS ENV HOMEBREW_PREFIX
  PATHS ${hdf5_binpref}
  PATH_SUFFIXES ${hdf5_binsuf}
)

if(HDF5_C_COMPILER_EXECUTABLE)
  get_flags(${HDF5_C_COMPILER_EXECUTABLE} c_raw)
  if(c_raw)
    pop_flag(${c_raw} -L lib_dirs)
    pop_flag(${c_raw} -I inc_dirs)
    if(NOT inc_dirs AND parallel IN_LIST HDF5_FIND_COMPONENTS)
      get_flags(${MPI_C_COMPILER} c_raw)
      if(c_raw)
        pop_flag(${c_raw} -I inc_dirs)
      endif(c_raw)
    endif()
  endif(c_raw)
endif()

if(inc_dirs)
  set(${inc_var} ${inc_dirs} PARENT_SCOPE)
endif()

if(lib_dirs)
  set(${lib_var} ${lib_dirs} PARENT_SCOPE)
endif()


endfunction(hdf5_c_wrap)


function(check_hdf5_link)

if(NOT HDF5_C_FOUND)
  return()
endif()

list(PREPEND CMAKE_REQUIRED_LIBRARIES ${HDF5_C_LIBRARIES})
set(CMAKE_REQUIRED_INCLUDES ${HDF5_C_INCLUDE_DIR})

if(HDF5_parallel_FOUND)
  list(APPEND CMAKE_REQUIRED_INCLUDES ${MPI_C_INCLUDE_DIRS})
  list(APPEND CMAKE_REQUIRED_LIBRARIES ${MPI_C_LIBRARIES})

  check_symbol_exists(H5Pset_fapl_mpio hdf5.h HAVE_H5Pset_fapl_mpio)
  if(NOT HAVE_H5Pset_fapl_mpio)
    return()
  endif()

  set(src [=[
  #include "hdf5.h"
  #include "mpi.h"

  int main(void){
  MPI_Init(NULL, NULL);

  hid_t plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5Pset_fapl_mpio(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);

  H5Pclose(plist_id);

  MPI_Finalize();

  return 0;
  }
  ]=])

else()
  set(src [=[
  #include "hdf5.h"

  int main(void){
  hid_t f = H5Fcreate("junk.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  herr_t status = H5Fclose (f);
  return 0;}
  ]=])
endif(HDF5_parallel_FOUND)

check_source_compiles(C "${src}" HDF5_C_links)

if(NOT HDF5_C_links)
  return()
endif()


if(HDF5_Fortran_FOUND)

list(PREPEND CMAKE_REQUIRED_LIBRARIES ${HDF5_Fortran_LIBRARIES})
set(CMAKE_REQUIRED_INCLUDES ${HDF5_Fortran_INCLUDE_DIR} ${HDF5_C_INCLUDE_DIR})

if(HDF5_parallel_FOUND)
  list(APPEND CMAKE_REQUIRED_INCLUDES ${MPI_Fortran_INCLUDE_DIRS})
  list(APPEND CMAKE_REQUIRED_LIBRARIES ${MPI_Fortran_LIBRARIES})

  set(src "program test_fortran_mpi
  use hdf5
  use mpi

  integer :: ierr
  integer(HID_T) :: plist_id

  call mpi_init(ierr)

  call h5open_f(ierr)

  call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
  call h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL, ierr)

  call h5pclose_f(plist_id, ierr)

  call mpi_finalize(ierr)

  end program")

  if(NOT DEFINED HDF5_Fortran_links)
    message(STATUS "Checking Fortran HDF5 MPI h5pset_fapl_mpio_f")
  endif()
else()
  set(src "program test_minimal
  use hdf5, only : h5open_f, h5close_f
  use h5lt, only : h5ltmake_dataset_f
  implicit none
  integer :: i
  call h5open_f(i)
  call h5close_f(i)
  end program")
endif()

check_source_compiles(Fortran ${src} HDF5_Fortran_links)

if(NOT HDF5_Fortran_links)
  return()
endif()

endif(HDF5_Fortran_FOUND)

set(HDF5_links true PARENT_SCOPE)

endfunction(check_hdf5_link)

# === main program

set(CMAKE_REQUIRED_LIBRARIES)

if(NOT HDF5_ROOT AND DEFINED ENV{HDF5_ROOT})
  set(HDF5_ROOT $ENV{HDF5_ROOT})
endif()

# we don't use pkg-config names because some distros pkg-config for HDF5 is broken
# however at least the paths are often correct
find_package(PkgConfig)
if(NOT HDF5_ROOT AND NOT HDF5_FOUND)
  if(parallel IN_LIST HDF5_FIND_COMPONENTS)
    pkg_search_module(pc_hdf5 hdf5-openmpi hdf5-mpich hdf5)
  else()
    pkg_search_module(pc_hdf5 hdf5-serial hdf5)
  endif()
endif()

set(hdf5_lsuf hdf5)
if(NOT parallel IN_LIST HDF5_FIND_COMPONENTS)
  list(PREPEND hdf5_lsuf hdf5/serial)
endif()

set(_psuf static)

set(hdf5_msuf static)
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  list(APPEND hdf5_msuf gfortran/modules)
  if(NOT HDF5_ROOT AND parallel IN_LIST HDF5_FIND_COMPONENTS)
    list(PREPEND hdf5_msuf gfortran/modules/openmpi gfortran/modules/mpich)
  endif()
endif()

set(hdf5_binpref)
if(CMAKE_SYSTEM_NAME STREQUAL Linux)
  set(hdf5_binpref /usr/lib64)
endif()

set(hdf5_binsuf bin)
if(NOT HDF5_ROOT AND parallel IN_LIST HDF5_FIND_COMPONENTS)
  list(APPEND hdf5_binsuf openmpi/bin mpich/bin)
endif()
# Not immediately clear the benefits of this, as we'd have to foreach()
# a priori names, kind of like we already do with find_library()
# find_package(hdf5 CONFIG)
# message(STATUS "hdf5 found ${hdf5_FOUND}")

if(Fortran IN_LIST HDF5_FIND_COMPONENTS)
  find_hdf5_fortran()
endif()

if(CXX IN_LIST HDF5_FIND_COMPONENTS)
  find_hdf5_cxx()
endif()

# C is always needed
find_hdf5_c()

# required libraries
if(HDF5_C_FOUND)
  detect_config()
endif(HDF5_C_FOUND)

# --- configure time checks
# these checks avoid messy, confusing errors at build time
check_hdf5_link()

set(CMAKE_REQUIRED_LIBRARIES)
set(CMAKE_REQUIRED_INCLUDES)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(HDF5
  REQUIRED_VARS HDF5_C_LIBRARIES HDF5_links
  VERSION_VAR HDF5_VERSION
  HANDLE_COMPONENTS)

if(HDF5_FOUND)
  set(HDF5_INCLUDE_DIRS ${HDF5_Fortran_INCLUDE_DIR} ${HDF5_CXX_INCLUDE_DIR} ${HDF5_C_INCLUDE_DIR})
  set(HDF5_LIBRARIES ${HDF5_Fortran_LIBRARIES} ${HDF5_CXX_LIBRARIES} ${HDF5_C_LIBRARIES})

  if(NOT TARGET HDF5::HDF5)
    add_library(HDF5::HDF5 INTERFACE IMPORTED)
    set_target_properties(HDF5::HDF5 PROPERTIES
      INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
      INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}")
    if(hdf5_have_zlib)
      target_link_libraries(HDF5::HDF5 INTERFACE ZLIB::ZLIB)
    endif()
    if(hdf5_have_szip)
      target_link_libraries(HDF5::HDF5 INTERFACE SZIP::SZIP)
    endif()

    if(Threads_FOUND)
      target_link_libraries(HDF5::HDF5 INTERFACE Threads::Threads)
    endif()

    target_link_libraries(HDF5::HDF5 INTERFACE ${CMAKE_DL_LIBS})

    if(UNIX)
      target_link_libraries(HDF5::HDF5 INTERFACE m)
    endif(UNIX)
  endif()
endif(HDF5_FOUND)

mark_as_advanced(HDF5_Fortran_LIBRARY HDF5_Fortran_HL_LIBRARY
HDF5_C_LIBRARY HDF5_C_HL_LIBRARY
HDF5_CXX_LIBRARY HDF5_CXX_HL_LIBRARY
HDF5_C_INCLUDE_DIR HDF5_CXX_INCLUDE_DIR HDF5_Fortran_INCLUDE_DIR)
