# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:

FindHDF5
---------

by SciVision www.scivision.dev

Finds HDF5 library for C, CXX, Fortran.
Finds serial or parallel HDF5.

Hints
^^^^^

``HDF5_NO_FIND_WRAPPER``
  can be set ``false`` (default ``true``) to not use the HDF5 compiler wrappers
  ``h5cc``, ``h5fc`` or similar, which can be useful if undesired compiler
  wrappers are present on the system.

``HDF5_USE_STATIC_LIBRARIES``
  if set ``true``,  prefers static libraries. Default is ``false``.

``HDF5_FIND_DEBUG_LIBRARIES``
  if set ``true``, prefers debug versions of the libraries. Default is
  ``false``.

If parallel HDF5 is desired or acceptable, the environment variable
``HDF5MPI_ROOT`` or CMake variable HDF5MPI_ROOT can specify the location of the
HDF5-MPI parallel library.


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

``HDF5_IS_PARALLEL``
  HDF5 links the MPI library (thus user program must link MPI as well)

Components
==========

``C``
  C is always required for HDF5, even if the user only needs C++ or Fortran,
  because the C library is a dependency of the C++ and Fortran libraries.
  The C library is also needed to get the config info for the link checks.

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


function(hdf5_get_flags exec outvar)

if(HDF5_USE_STATIC_LIBRARIES)
  set(opt "-noshlib")
else()
  set(opt "-shlib")
endif()

execute_process(COMMAND ${exec} -show ${opt}
OUTPUT_STRIP_TRAILING_WHITESPACE
OUTPUT_VARIABLE ret
RESULT_VARIABLE code
TIMEOUT 10
ERROR_QUIET
)

if(code EQUAL 0)
  set(${outvar} ${ret} PARENT_SCOPE)
endif()

endfunction()


function(hdf5_pop_flag raw flag outvar)
# this gives the argument to flags to get their paths like -I or -l or -L

set(_v)
string(REGEX MATCHALL "(^| )${flag} *([^\" ]+|\"[^\"]+\")" _vars "${raw}")
foreach(_p IN LISTS _vars)
  string(REGEX REPLACE "(^| )${flag} *" "" _p "${_p}")
  list(APPEND _v "${_p}")
endforeach()

set(${outvar} ${_v} PARENT_SCOPE)

endfunction()


macro(hdf5_find_mpi)
# non-cache set by FindMPI are not visible outside function -- need macro just to see within that function
set(mpi_comp C)
if(Fortran IN_LIST HDF5_FIND_COMPONENTS)
  list(APPEND mpi_comp Fortran)
endif()
if(HDF5_FIND_REQUIRED)
  find_package(MPI COMPONENTS ${mpi_comp} REQUIRED)
else()
  find_package(MPI COMPONENTS ${mpi_comp})
endif()

endmacro()


macro(hdf5_detect_config path)

hdf5_msvc_workaround()

set(CMAKE_REQUIRED_INCLUDES ${path} ${HDF5_C_INCLUDE_DIR})

find_file(HDF5_CONFIG_FILE
NAMES H5pubconf.h H5pubconf-64.h
HINTS ${CMAKE_REQUIRED_INCLUDES}
PATH_SUFFIXES ${hdf5_isuf}
NO_DEFAULT_PATH
)
message(VERBOSE "HDF5 config: ${HDF5_CONFIG_FILE}")

if(NOT HDF5_CONFIG_FILE)
  return()
endif()

# Always check for HDF5 MPI support because HDF5 link fails if MPI is linked into HDF5.
check_symbol_exists(H5_HAVE_PARALLEL ${HDF5_CONFIG_FILE} HDF5_IS_PARALLEL)

set(HDF5_parallel_FOUND false)
if(HDF5_IS_PARALLEL)
  hdf5_find_mpi()
  if(NOT MPI_FOUND)
    return()
  endif()

  set(HDF5_parallel_FOUND true)
endif()

# get HDF5 version from factory CMake Modules/FindHDF5.cmake
file(STRINGS ${HDF5_CONFIG_FILE} _def
 REGEX "^[ \t]*#[ \t]*define[ \t]+H5_VERSION[ \t]+"
)
message(DEBUG "HDF5 version define: ${_def}")

if("${_def}" MATCHES "H5_VERSION[ \t]+\"([0-9]+\\.[0-9]+\\.[0-9]+)")
  set(HDF5_VERSION "${CMAKE_MATCH_1}")
endif()
message(DEBUG "HDF5 version match 0, 1: ${CMAKE_MATCH_0}   ${CMAKE_MATCH_1}")

endmacro()


macro(hdf5_lib_pref)

if(MSVC)
  # quirk of HDF5 1.x and 2.x packaging - they use a "lib" prefix even for MSVC-like.
  list(PREPEND CMAKE_FIND_LIBRARY_PREFIXES lib)
endif()

if(HDF5_USE_STATIC_LIBRARIES)
  # for MSVC, there is always a .lib, even for shared libraries, but the .lib points to a bin/.dll
  if(NOT MSVC)
    list(PREPEND CMAKE_FIND_LIBRARY_SUFFIXES .a)
  endif()
endif()

endmacro()


function(hdf5_find_fortran)

if(parallel IN_LIST HDF5_FIND_COMPONENTS AND NOT HDF5_parallel_FOUND)
  # this avoids expensive Fortran find when MPI isn't linked properly
  return()
endif()

if(HDF5_NO_FIND_WRAPPER)
  set(hdf5_lib_dirs)
  set(hdf5_inc_dirs)
else()
  hdf5_fortran_wrap(hdf5_lib_dirs hdf5_inc_dirs)
endif()

hdf5_lib_pref()

set(_names hdf5_fortran)
set(_hl_names hdf5_hl_fortran hdf5hl_fortran)
set(_hl_stub_names hdf5_hl_f90cstub)
set(_stub_names hdf5_f90cstub)

# distro names (Ubuntu)
if(HDF5_parallel_FOUND)
  list(APPEND _names hdf5_openmpi_fortran hdf5_mpich_fortran)
  list(APPEND _hl_names hdf5_openmpihl_fortran hdf5_mpichhl_fortran)
else()
  list(APPEND _names hdf5_serial_fortran)
  list(APPEND _hl_names hdf5_serialhl_fortran)
endif()

# Debug names
if(HDF5_FIND_DEBUG_LIBRARIES)
if(MSVC)
  list(APPEND _names hdf5_fortran_D)
  list(APPEND _hl_names hdf5_hl_fortran_D)
  list(APPEND _hl_stub_names hdf5_hl_f90cstub_D)
  list(APPEND _stub_names hdf5_f90cstub_D)
else()
  list(APPEND _names hdf5_fortran_debug)
  list(APPEND _hl_names hdf5_hl_fortran_debug)
  list(APPEND _hl_stub_names hdf5_hl_f90cstub_debug)
  list(APPEND _stub_names hdf5_f90cstub_debug)
endif()
endif()

find_library(HDF5_Fortran_LIBRARY
NAMES ${_names}
HINTS ${hdf5_root} ${hdf5_lib_dirs}
PATH_SUFFIXES ${hdf5_lsuf}
DOC "HDF5 Fortran API"
${_hdf5_nodef}
)

cmake_path(GET HDF5_Fortran_LIBRARY PARENT_PATH hdf5_libdir)

find_library(HDF5_Fortran_HL_LIBRARY
NAMES ${_hl_names}
HINTS ${hdf5_libdir}
NO_DEFAULT_PATH
DOC "HDF5 Fortran HL high-level API"
)

# not all platforms have this stub
find_library(HDF5_Fortran_HL_stub
NAMES ${_hl_stub_names}
HINTS ${hdf5_libdir}
NO_DEFAULT_PATH
DOC "Fortran C HL interface, not all HDF5 implementations have/need this"
)

find_library(HDF5_Fortran_stub
NAMES ${_stub_names}
HINTS ${hdf5_libdir}
NO_DEFAULT_PATH
DOC "Fortran C interface, not all HDF5 implementations have/need this"
)

set(HDF5_Fortran_LIBRARIES ${HDF5_Fortran_HL_LIBRARY} ${HDF5_Fortran_LIBRARY})
# must consider each stub separately as only one may be present
if(HDF5_Fortran_HL_stub)
  list(APPEND HDF5_Fortran_LIBRARIES ${HDF5_Fortran_HL_stub})
endif()
if(HDF5_Fortran_stub)
  list(APPEND HDF5_Fortran_LIBRARIES ${HDF5_Fortran_stub})
endif()

if(HDF5_parallel_FOUND)
  # HDF5-MPI system library presents a unique challenge, as when non-MPI HDF5 is
  # also installed, which is typically necessary for other system libraries, the
  # HDF5-MPI compiler wrapper often includes that wrong non-MPI include dir first.
  # The most general approach seemed to be the following:
  # search in a for loop and do a link check.
  if(NOT HDF5_Fortran_INCLUDE_DIR)
    foreach(i IN LISTS hdf5_root HDF5_C_INCLUDE_DIR hdf5_inc_dirs)

      find_path(HDF5_Fortran_INCLUDE_DIR
      NAMES hdf5.mod
      NO_DEFAULT_PATH
      HINTS ${i}
      PATH_SUFFIXES ${hdf5_msuf_hl}
      DOC "HDF5 Fortran module path"
      )

      find_path(HDF5_Fortran_HL_INCLUDE_DIR
      NAMES h5lt.mod
      NO_DEFAULT_PATH
      HINTS ${HDF5_Fortran_INCLUDE_DIR}
      PATH_SUFFIXES ${hdf5_msuf_hl}
      DOC "HDF5 Fortran HL module path"
      VALIDATOR hdf5_fortran_validate
      )
    endforeach()
  endif()

  if(NOT HDF5_Fortran_INCLUDE_DIR)
    # last resort, might give incompatible non-MPI hdf5.mod
    find_path(HDF5_Fortran_INCLUDE_DIR
    NAMES hdf5.mod
    HINTS ${HDF5_C_INCLUDE_DIR} ${hdf5_inc_dirs}
    PATHS ${hdf5_binpref}
    PATH_SUFFIXES ${hdf5_msuf} ${hdf5_msuf_hl}
    DOC "HDF5 Fortran module path"
    )

    find_path(HDF5_Fortran_HL_INCLUDE_DIR
    NAMES h5lt.mod
    NO_DEFAULT_PATH
    HINTS ${HDF5_Fortran_INCLUDE_DIR}
    PATH_SUFFIXES ${hdf5_msuf_hl}
    DOC "HDF5 Fortran HL module path"
    VALIDATOR hdf5_fortran_validate
    )
  endif()
else()
  find_path(HDF5_Fortran_INCLUDE_DIR
  NAMES hdf5.mod
  HINTS ${hdf5_root} ${HDF5_C_INCLUDE_DIR} ${hdf5_inc_dirs}
  PATHS ${hdf5_binpref}
  PATH_SUFFIXES ${hdf5_msuf} ${hdf5_msuf_hl}
  DOC "HDF5 Fortran module path"
  ${_hdf5_nodef}
  )
  if(HDF5_Fortran_INCLUDE_DIR)
    find_path(HDF5_Fortran_HL_INCLUDE_DIR
    NAMES h5lt.mod
    NO_DEFAULT_PATH
    HINTS ${HDF5_Fortran_INCLUDE_DIR}
    PATH_SUFFIXES ${hdf5_msuf_hl}
    DOC "HDF5 Fortran HL module path"
    VALIDATOR hdf5_fortran_validate
    )
  endif()
endif()

if(HDF5_Fortran_LIBRARY AND HDF5_Fortran_HL_LIBRARY AND HDF5_Fortran_INCLUDE_DIR AND HDF5_Fortran_HL_INCLUDE_DIR)
  set(HDF5_Fortran_LIBRARIES ${HDF5_Fortran_LIBRARIES} PARENT_SCOPE)
  set(HDF5_Fortran_FOUND true PARENT_SCOPE)
  set(HDF5_HL_FOUND true PARENT_SCOPE)
endif()

endfunction(hdf5_find_fortran)


function(hdf5_find_cxx)

if(parallel IN_LIST HDF5_FIND_COMPONENTS AND NOT HDF5_parallel_FOUND)
  # avoid expensive C++ find when MPI isn't linked properly
  return()
endif()

if(HDF5_NO_FIND_WRAPPER)
  set(hdf5_lib_dirs)
  set(hdf5_inc_dirs)
else()
  hdf5_cxx_wrap(hdf5_lib_dirs hdf5_inc_dirs)
endif()

hdf5_lib_pref()

set(_names hdf5_cpp)
set(_hl_names hdf5_hl_cpp)

# distro names (Ubuntu)
if(HDF5_parallel_FOUND)
  list(APPEND _names hdf5_openmpi_cpp hdf5_mpich_cpp)
  list(APPEND _hl_names hdf5_openmpi_hl_cpp hdf5_mpich_hl_cpp)
else()
  list(APPEND _names hdf5_serial_cpp)
  list(APPEND _hl_names hdf5_serial_hl_cpp)
endif()

# Debug names
if(HDF5_FIND_DEBUG_LIBRARIES)
if(MSVC)
  list(APPEND _names hdf5_cpp_D)
  list(APPEND _hl_names hdf5_hl_cpp_D)
else()
  list(APPEND _names hdf5_cpp_debug)
  list(APPEND _hl_names hdf5_hl_cpp_debug)
endif()
endif()

find_library(HDF5_CXX_LIBRARY
NAMES ${_names}
HINTS ${hdf5_root} ${hdf5_lib_dirs}
PATH_SUFFIXES ${hdf5_lsuf}
DOC "HDF5 C++ API"
${_hdf5_nodef}
)

cmake_path(GET HDF5_CXX_LIBRARY PARENT_PATH hdf5_libdir)

find_library(HDF5_CXX_HL_LIBRARY
NAMES ${_hl_names}
HINTS ${hdf5_libdir}
NO_DEFAULT_PATH
DOC "HDF5 C++ high-level API"
)

find_path(HDF5_CXX_INCLUDE_DIR
NAMES hdf5.h
HINTS ${HDF5_C_INCLUDE_DIR} ${hdf5_root} ${hdf5_inc_dirs}
PATH_SUFFIXES ${hdf5_isuf}
DOC "HDF5 C header"
${_hdf5_nodef}
)

if(HDF5_CXX_LIBRARY AND HDF5_CXX_HL_LIBRARY AND HDF5_CXX_INCLUDE_DIR)
  set(HDF5_CXX_LIBRARIES ${HDF5_CXX_HL_LIBRARY} ${HDF5_CXX_LIBRARY} PARENT_SCOPE)
  set(HDF5_CXX_FOUND true PARENT_SCOPE)
  set(HDF5_HL_FOUND true PARENT_SCOPE)
endif()

endfunction(hdf5_find_cxx)


function(hdf5_find_c)

if(HDF5_NO_FIND_WRAPPER)
  set(hdf5_lib_dirs)
  set(hdf5_inc_dirs)
else()
  hdf5_c_wrap(hdf5_lib_dirs hdf5_inc_dirs support_libs)
endif()

find_path(HDF5_C_INCLUDE_DIR
NAMES hdf5.h
HINTS ${hdf5_root} ${hdf5_inc_dirs}
PATH_SUFFIXES ${hdf5_isuf}
DOC "HDF5 C header"
${_hdf5_nodef}
)
if(HDF5_C_INCLUDE_DIR)
  find_path(HDF5_C_HL_INCLUDE_DIR
    NAMES hdf5_hl.h
    NO_DEFAULT_PATH
    HINTS ${HDF5_C_INCLUDE_DIR}
    PATH_SUFFIXES ${hdf5_isuf}
    DOC "HDF5 C HL header"
  )
endif()

if(NOT HDF5_C_INCLUDE_DIR)
  return()
endif()

set(_names hdf5)
set(_hl_names hdf5_hl)

# distro names (Ubuntu)
if(parallel IN_LIST HDF5_FIND_COMPONENTS)
  list(APPEND _names hdf5_openmpi hdf5_mpich)
  list(APPEND _hl_names hdf5_openmpi_hl hdf5_mpich_hl)
else()
  list(APPEND _names hdf5_serial)
  list(APPEND _hl_names hdf5_serial_hl)
endif()

# debug names
if(HDF5_FIND_DEBUG_LIBRARIES)
if(MSVC)
  list(APPEND _names hdf5_D)
  list(APPEND _hl_names hdf5_hl_D)
else()
  list(APPEND _names hdf5_debug)
  list(APPEND _hl_names hdf5_hl_debug)
endif()
endif()

message(DEBUG "HDF5 C library search suffixes: ${CMAKE_FIND_LIBRARY_SUFFIXES}")

set(CMAKE_REQUIRED_LIBRARIES)
foreach(_lib IN LISTS support_libs)
  find_library(HDF5_C_LIBRARY_${_lib}
  NAMES ${_lib}
  DOC "HDF5 C library dependency ${_lib} from ${HDF5_C_COMPILER_EXECUTABLE} wrapper"
  )
  if(HDF5_C_LIBRARY_${_lib})
    list(APPEND CMAKE_REQUIRED_LIBRARIES ${HDF5_C_LIBRARY_${_lib}})
  endif()
endforeach()

# this must be AFTER finding the support_libs, as the support libs should be system default i.e. shared even if HDF5 is static.
# if one REALLY wants all static, there are specific compiler flags etc. that are beyond the scope of a CMake Find module.
hdf5_lib_pref()

find_library(HDF5_C_LIBRARY
NAMES ${_names}
HINTS ${hdf5_root} ${hdf5_lib_dirs}
PATH_SUFFIXES ${hdf5_lsuf}
DOC "HDF5 C library (necessary for all languages)"
VALIDATOR hdf5_c_validate
${_hdf5_nodef}
)

if(HDF5_C_LIBRARY)
cmake_path(GET HDF5_C_LIBRARY PARENT_PATH hdf5_libdir)

find_library(HDF5_C_HL_LIBRARY
NAMES ${_hl_names}
HINTS ${hdf5_libdir}
NO_DEFAULT_PATH
DOC "HDF5 C high level interface"
)
endif()

if(HDF5_C_HL_LIBRARY AND HDF5_C_LIBRARY)
  set(HDF5_C_LIBRARIES ${HDF5_C_HL_LIBRARY} ${HDF5_C_LIBRARY} ${CMAKE_REQUIRED_LIBRARIES} PARENT_SCOPE)
  set(HDF5_C_FOUND true PARENT_SCOPE)
  set(HDF5_HL_FOUND true PARENT_SCOPE)
endif()

endfunction()


function(hdf5_fortran_wrap lib_var inc_var)

set(lib_dirs)
set(inc_dirs)

if(HDF5_parallel_FOUND)
  set(wrapper_names h5pfc h5pfc.openmpi h5pfc.mpich)
else()
  set(wrapper_names h5fc)
endif()

find_program(HDF5_Fortran_COMPILER_EXECUTABLE
NAMES ${wrapper_names}
HINTS ${hdf5_root}
PATHS ${hdf5_binpref}
PATH_SUFFIXES ${hdf5_binsuf}
DOC "HDF5 Fortran compiler script"
${_hdf5_nodef}
)

if(NOT HDF5_Fortran_COMPILER_EXECUTABLE)
  return()
endif()

hdf5_get_flags(${HDF5_Fortran_COMPILER_EXECUTABLE} f_raw)
if(f_raw)
  hdf5_pop_flag(${f_raw} -L lib_dirs)
  hdf5_pop_flag(${f_raw} -I inc_dirs)
  if(NOT inc_dirs AND parallel IN_LIST HDF5_FIND_COMPONENTS)
    hdf5_get_flags(${MPI_Fortran_COMPILER} f_raw)
    if(f_raw)
      hdf5_pop_flag(${f_raw} -I inc_dirs)
    endif(f_raw)
  endif()
endif(f_raw)

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

if(HDF5_parallel_FOUND)
  set(wrapper_names h5c++.openmpi h5c++.mpich)
else()
  set(wrapper_names h5c++)
endif()

find_program(HDF5_CXX_COMPILER_EXECUTABLE
NAMES ${wrapper_names}
HINTS ${hdf5_root}
PATHS ${hdf5_binpref}
PATH_SUFFIXES ${hdf5_binsuf}
DOC "HDF5 C++ compiler script"
${_hdf5_nodef}
)

if(NOT HDF5_CXX_COMPILER_EXECUTABLE)
  return()
endif()

hdf5_get_flags(${HDF5_CXX_COMPILER_EXECUTABLE} cxx_raw)
if(cxx_raw)
  hdf5_pop_flag(${cxx_raw} -L lib_dirs)
  hdf5_pop_flag(${cxx_raw} -I inc_dirs)
endif(cxx_raw)

if(inc_dirs)
  set(${inc_var} ${inc_dirs} PARENT_SCOPE)
endif()

if(lib_dirs)
  set(${lib_var} ${lib_dirs} PARENT_SCOPE)
endif()

endfunction()


function(hdf5_c_wrap lib_var inc_var support_var)

set(lib_dirs)
set(inc_dirs)

if(parallel IN_LIST HDF5_FIND_COMPONENTS)
  set(wrapper_names h5pcc h5pcc.openmpi h5pcc.mpich)
else()
  set(wrapper_names h5cc)
endif()

find_program(HDF5_C_COMPILER_EXECUTABLE
NAMES ${wrapper_names}
HINTS ${hdf5_root}
PATHS ${hdf5_binpref}
PATH_SUFFIXES ${hdf5_binsuf}
DOC "HDF5 C compiler script"
${_hdf5_nodef}
)

if(NOT HDF5_C_COMPILER_EXECUTABLE)
  return()
endif()

hdf5_get_flags(${HDF5_C_COMPILER_EXECUTABLE} c_raw)
if(c_raw)

hdf5_pop_flag(${c_raw} -L lib_dirs)
if(lib_dirs)
  set(${lib_var} ${lib_dirs} PARENT_SCOPE)
endif()

hdf5_pop_flag(${c_raw} -l support_libs)
list(FILTER support_libs EXCLUDE REGEX "^(hdf5|hdf5_hl)$")
if(support_libs)
  set(${support_var} ${support_libs} PARENT_SCOPE)
endif()

hdf5_pop_flag(${c_raw} -I inc_dirs)
if(NOT inc_dirs AND parallel IN_LIST HDF5_FIND_COMPONENTS)
  hdf5_get_flags(${MPI_C_COMPILER} c_raw)
  if(c_raw)
    hdf5_pop_flag(${c_raw} -I inc_dirs)
  endif(c_raw)
endif()
if(inc_dirs)
  set(${inc_var} ${inc_dirs} PARENT_SCOPE)
endif()

endif()

endfunction()


function(hdf5_c_validate _result path)

list(PREPEND CMAKE_REQUIRED_LIBRARIES ${path})
set(CMAKE_REQUIRED_INCLUDES ${HDF5_C_INCLUDE_DIR})

cmake_path(GET path PARENT_PATH ppath)
hdf5_detect_config(${ppath})

if(HDF5_parallel_FOUND)
  hdf5_find_mpi()

  list(APPEND CMAKE_REQUIRED_LIBRARIES MPI::MPI_C)

  check_symbol_exists(H5Pset_fapl_mpio hdf5.h HAVE_H5Pset_fapl_mpio)
  if(NOT HAVE_H5Pset_fapl_mpio)
    set(${_result} false PARENT_SCOPE)
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
  if(H5open() != 0) return 1;
  if(H5F_ACC_RDONLY == H5F_ACC_TRUNC || H5F_ACC_RDONLY == H5F_ACC_RDWR) return 1;
  if(H5close() != 0) return 1;
  return 0;
  }
  ]=])
endif(HDF5_parallel_FOUND)

string(MAKE_C_IDENTIFIER ${path} uniqpath)

check_source_compiles(C "${src}" HDF5_C_${uniqpath}_links)

set(${_result} ${HDF5_C_${uniqpath}_links} PARENT_SCOPE)

message(DEBUG "HDF5 C link check: ${HDF5_C_${uniqpath}_links} ${CMAKE_REQUIRED_LIBRARIES} ${CMAKE_REQUIRED_INCLUDES}")

endfunction()


function(hdf5_fortran_validate _result path)

cmake_path(GET path PARENT_PATH ppath)

set(CMAKE_REQUIRED_LIBRARIES ${HDF5_Fortran_LIBRARIES} ${HDF5_C_LIBRARIES})
# C and C_HL library are always needed for Fortran_HL
# use HDF5_Fortran_LIBRARIES as it has the stubs if needed

set(CMAKE_REQUIRED_INCLUDES ${ppath} ${HDF5_Fortran_INCLUDE_DIR})
# need HDF5_Fortran_INCLUDE_DIR because with HDF5 2.x, it may be distinct from HDF5_Fortran_HL_INCLUDE_DIR,
# and the test program needs to include both hdf5.mod and h5lt.mod

hdf5_detect_config(${ppath})

if(HDF5_parallel_FOUND)
  hdf5_find_mpi()

  list(APPEND CMAKE_REQUIRED_LIBRARIES MPI::MPI_Fortran)

  set(src "program test
  use hdf5
  use mpi
  implicit none
  integer :: ierr, mpi_id
  integer(HID_T) :: fapl, xfer_id
  call mpi_init(ierr)
  call h5open_f(ierr)
  call h5pcreate_f(H5P_FILE_ACCESS_F, fapl, ierr)
  call h5pset_fapl_mpio_f(fapl, MPI_COMM_WORLD, MPI_INFO_NULL, ierr)
  call h5pcreate_f(H5P_DATASET_XFER_F, xfer_id, ierr)
  call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_COLLECTIVE_F, ierr)
  call mpi_finalize(ierr)
  end program")
else()
  set(src "program test_minimal
  use hdf5
  use h5lt

  implicit none

  integer :: i, p
  integer(HID_T) :: lid
  character(*), parameter :: filename='test_minimal.h5'

  p = 42

  call H5open_f(i)
  call H5Fcreate_f(filename, H5F_ACC_TRUNC_F, lid, i)
  call H5LTmake_dataset_f(lid, 'A', 0, shape(p, kind=HSIZE_T), h5kind_to_type(kind(p),H5_INTEGER_KIND), p, i)
  call H5Fclose_f(lid, i)
  call H5close_f(i)

  end program")
endif()

string(MAKE_C_IDENTIFIER ${path} uniqpath)

check_source_compiles(Fortran ${src} HDF5_Fortran_${uniqpath}_links)

set(${_result} ${HDF5_Fortran_${uniqpath}_links} PARENT_SCOPE)

message(DEBUG "HDF5 Fortran link check: ${HDF5_Fortran_${uniqpath}_links} ${CMAKE_REQUIRED_LIBRARIES} ${CMAKE_REQUIRED_INCLUDES}")

endfunction(hdf5_fortran_validate)


macro(hdf5_msvc_workaround)

# HDF5 bug #3663 for HDF5 1.14.2..2.1
# https://github.com/HDFGroup/hdf5/issues/3663
# we have it here too so that the link test works without the *targets.cmake files
if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
  message(DEBUG "HDF5: applying workaround for HDFGroup/HDF5 bug Issue 3663 with Intel oneAPI on Windows")
  list(APPEND CMAKE_REQUIRED_LIBRARIES shlwapi)
endif()

endmacro(hdf5_msvc_workaround)

# === main program

if(NOT DEFINED HDF5_NO_FIND_WRAPPER)
  set(HDF5_NO_FIND_WRAPPER false)
endif()

if(NOT HDF5MPI_ROOT AND DEFINED ENV{HDF5MPI_ROOT})
  set(HDF5MPI_ROOT $ENV{HDF5MPI_ROOT})
endif()

if(HDF5MPI_ROOT AND parallel IN_LIST HDF5_FIND_COMPONENTS)
  set(hdf5_root ${HDF5MPI_ROOT})
elseif(HDF5_ROOT)
  set(hdf5_root ${HDF5_ROOT})
elseif(DEFINED ENV{HDF5_ROOT})
  set(hdf5_root $ENV{HDF5_ROOT})
endif()

# This is necessary to avoid bizarre paths when relative path is given for HDF5_ROOT
if(hdf5_root)
  file(REAL_PATH "${hdf5_root}" hdf5_root EXPAND_TILDE)
  set(_hdf5_nodef NO_DEFAULT_PATH)
endif()

# --- library suffixes

set(hdf5_lsuf lib hdf5/lib)  # need explicit "lib" for self-built HDF5
if(NOT hdf5_root)
  list(PREPEND hdf5_lsuf hdf5/openmpi hdf5/mpich)  # Ubuntu
  list(PREPEND hdf5_lsuf openmpi/lib mpich/lib)  # CentOS
  if(NOT parallel IN_LIST HDF5_FIND_COMPONENTS)
    list(PREPEND hdf5_lsuf hdf5/serial)  # Ubuntu
  endif()
endif()

# --- include and modules suffixes
set(hdf5_isuf include)
set(hdf5_msuf include)
if(HDF5_USE_STATIC_LIBRARIES)
  list(PREPEND hdf5_isuf static)
  list(PREPEND hdf5_msuf static)
  set(hdf5_msuf_hl mod/static)
else()
  # have shared and static because on Windows it's ambiguous which library type is being used
  list(PREPEND hdf5_isuf shared static)
  list(PREPEND hdf5_msuf shared static)
  set(hdf5_msuf_hl mod/shared mod/static)
endif()

# Ubuntu
list(PREPEND hdf5_isuf hdf5/openmpi hdf5/mpich)
list(PREPEND hdf5_msuf hdf5/openmpi hdf5/mpich)

if(NOT parallel IN_LIST HDF5_FIND_COMPONENTS)
  # Ubuntu
  list(PREPEND hdf5_isuf hdf5/serial)
  list(PREPEND hdf5_msuf hdf5/serial)
endif()

if(CMAKE_SYSTEM_PROCESSOR MATCHES "(x86_64|AMD64)")
  list(APPEND hdf5_isuf openmpi-x86_64 mpich-x86_64)  # CentOS
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "(aarch64|arm64)")
  list(APPEND hdf5_isuf openmpi-aarch64 mpich-aarch64)  # CentOS
endif()

if(NOT hdf5_root AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  # CentOS paths
  if(parallel IN_LIST HDF5_FIND_COMPONENTS)
    list(PREPEND hdf5_msuf gfortran/modules/openmpi gfortran/modules/mpich)
  else()
    list(APPEND hdf5_msuf gfortran/modules)
  endif()
endif()

# --- binary prefix / suffix
set(hdf5_binpref)
if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  set(hdf5_binpref /usr/lib64)
endif()

set(hdf5_binsuf bin)
if(NOT hdf5_root AND parallel IN_LIST HDF5_FIND_COMPONENTS)
  # CentOS paths
  list(APPEND hdf5_binsuf openmpi/bin mpich/bin)
endif()

# ----
# May not help, as we'd have to foreach() a priori names, like we already do with find_library()
# find_package(hdf5 CONFIG)
# ----

hdf5_find_c()
# HDF5 C and C_HL are always needed for C++ and Fortran, so find them unconditionally to get the config info for the link checks.

if(CXX IN_LIST HDF5_FIND_COMPONENTS)
  hdf5_find_cxx()
endif()

if(Fortran IN_LIST HDF5_FIND_COMPONENTS)
  hdf5_find_fortran()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(HDF5
VERSION_VAR HDF5_VERSION
HANDLE_COMPONENTS
)

if(HDF5_FOUND)
  set(HDF5_INCLUDE_DIRS ${HDF5_Fortran_INCLUDE_DIR} ${HDF5_Fortran_HL_INCLUDE_DIR} ${HDF5_CXX_INCLUDE_DIR} ${HDF5_C_INCLUDE_DIR})
  list(REMOVE_DUPLICATES HDF5_INCLUDE_DIRS)

  set(HDF5_LIBRARIES ${HDF5_Fortran_LIBRARIES} ${HDF5_CXX_LIBRARIES} ${HDF5_C_LIBRARIES})

  if(NOT TARGET HDF5::HDF5)
    add_library(HDF5::HDF5 INTERFACE IMPORTED)
    set_target_properties(HDF5::HDF5 PROPERTIES
      INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
      INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}")
  endif()

  if(NOT TARGET hdf5::hdf5_hl_fortran)
    if(HDF5_Fortran_HL_LIBRARY)
      add_library(hdf5::hdf5_hl_fortran INTERFACE IMPORTED)
      set_target_properties(hdf5::hdf5_hl_fortran PROPERTIES
        INTERFACE_LINK_LIBRARIES "${HDF5_Fortran_HL_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${HDF5_Fortran_HL_INCLUDE_DIR}")
    endif()
  endif()

  if(NOT TARGET hdf5::hdf5_fortran)
    if(HDF5_Fortran_LIBRARY)
      add_library(hdf5::hdf5_fortran INTERFACE IMPORTED)
      set_target_properties(hdf5::hdf5_fortran PROPERTIES
        INTERFACE_LINK_LIBRARIES "${HDF5_Fortran_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${HDF5_Fortran_INCLUDE_DIR}")
    endif()
  endif()

  if(NOT TARGET hdf5::hdf5_hl)
    if(HDF5_C_HL_LIBRARY)
      add_library(hdf5::hdf5_hl INTERFACE IMPORTED)
      set_target_properties(hdf5::hdf5_hl PROPERTIES
        INTERFACE_LINK_LIBRARIES "${HDF5_C_HL_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${HDF5_C_HL_INCLUDE_DIR}")
    endif()
  endif()

  if(NOT TARGET hdf5::hdf5)
    if(HDF5_C_LIBRARY)
      add_library(hdf5::hdf5 INTERFACE IMPORTED)
      set_target_properties(hdf5::hdf5 PROPERTIES
        INTERFACE_LINK_LIBRARIES "${HDF5_C_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${HDF5_C_INCLUDE_DIR}")
    endif()
  endif()

  if(NOT TARGET hdf5::hdf5_hl_cpp)
    if(HDF5_CXX_HL_LIBRARY)
      add_library(hdf5::hdf5_hl_cpp INTERFACE IMPORTED)
      set_target_properties(hdf5::hdf5_hl_cpp PROPERTIES
        INTERFACE_LINK_LIBRARIES "${HDF5_CXX_HL_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${HDF5_CXX_INCLUDE_DIR}")
    endif()
  endif()

  if(NOT TARGET hdf5::hdf5_cpp)
    if(HDF5_CXX_LIBRARY)
      add_library(hdf5::hdf5_cpp INTERFACE IMPORTED)
      set_target_properties(hdf5::hdf5_cpp PROPERTIES
        INTERFACE_LINK_LIBRARIES "${HDF5_CXX_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${HDF5_CXX_INCLUDE_DIR}")
    endif()
  endif()
endif(HDF5_FOUND)

mark_as_advanced(HDF5_Fortran_LIBRARY HDF5_Fortran_HL_LIBRARY
HDF5_C_LIBRARY HDF5_C_HL_LIBRARY HDF5_C_INCLUDE_DIR HDF5_C_HL_INCLUDE_DIR
HDF5_CXX_LIBRARY HDF5_CXX_HL_LIBRARY HDF5_CXX_INCLUDE_DIR
HDF5_Fortran_INCLUDE_DIR HDF5_Fortran_HL_INCLUDE_DIR
)
