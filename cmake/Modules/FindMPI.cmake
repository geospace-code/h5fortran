# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindMPI
-------

by Michael Hirsch www.scivision.dev

Finds compiler flags or library necessary to use MPI library (MPICH, OpenMPI, MS-MPI, Intel MPI, ...)

Components
==========

MPI code languages are specified by components:

``C``
  C interface for MPI--all MPI libraries have this. Default.

``CXX``
  C++ interface for MPI (not all MPI libraries have C++ interface)

``Fortran``
  Fortran interface for interface for MPI (some MPI libraries don't build this by default)


Result Variables
^^^^^^^^^^^^^^^^

``MPI_FOUND``
  indicates MPI library found

``MPI_LIBRARIES``
  MPI library path

``MPI_INCLUDE_DIRS``
  MPI include path

``MPI_<LANG>_LIBRARIES``
  libraries for <LANG>

``MPI_<LANG>_INCLUDE_DIRS``
  include dirs for <LANG>

``MPI_<LANG>_LINK_FLAGS``
  link flags for <LANG>

``MPI_<LANG>_COMPILER``
  compiler wrapper for <LANG>

``MPI_Fortran_HAVE_F90_MODULE``
  has MPI-2 Fortran 90 interface

``MPI_Fortran_HAVE_F08_MODULE``
  has MPI-3 Fortran 2008 interface

Imported Targets
^^^^^^^^^^^^^^^^

``MPI::MPI_<LANG>``
  imported target for <LANG>  e.g. ``MPI::MPI_C`` or ``MPI::MPI_Fortran``


#]=======================================================================]
include(CheckSourceCompiles)

set(CMAKE_REQUIRED_FLAGS)


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


function(get_link_flags raw outvar)


string(REGEX MATCHALL "(^| )(${CMAKE_LIBRARY_PATH_FLAG})([^\" ]+|\"[^\"]+\")" _Lflags "${raw}")
list(TRANSFORM _Lflags STRIP)
set(_flags ${_Lflags})

# check if compiler absolute path is first element and remove
if("${raw}" MATCHES "^/")
  if("${_flags}" MATCHES "^/")
    list(REMOVE_AT _flags 0)
  endif()
endif()

# Linker flags "-Wl,..."
string(REGEX MATCHALL "(^| )(-Wl,)([^\" ]+|\"[^\"]+\")" _Wflags "${raw}")
list(TRANSFORM _Wflags STRIP)
if(_Wflags)
  # this transform avoids CMake stripping out all "-Wl,rpath" after first.
  # Example:
  #  -Wl,rpath -Wl,/path/to/A -Wl,rpath -Wl,/path/to/B
  # becomes
  #  -Wl,-rpath,/path/to/A -Wl,-rpath,/path/to/B
  list(TRANSFORM _Wflags REPLACE "-Wl," "LINKER:")

  list(LENGTH _Wflags L)
  math(EXPR L "${L}-1")
  set(_work)
  set(skip -1)
  foreach(i RANGE ${L})
    list(GET _Wflags ${i} f)
    if("${f}" MATCHES "(^| )(LINKER:)([^\" ]+|\"[^\"]+\")")
      # attach to prior rpath
      if("${CMAKE_MATCH_3}" STREQUAL "-rpath")
        # "LINKER:-rpath" with path as next argument
        math(EXPR j "${i}+1")
        list(GET _Wflags ${j} fp)
        string(SUBSTRING "${fp}" 7 -1 p) # path without LINKER: prefix

        if(IS_DIRECTORY "${p}")
          # it's an rpath,directory so skip this flag next iteration
          string(APPEND f ",${p}")
          set(skip ${j})
        else()
          # if not a directory, just append it in the next iteration
        endif()
      elseif(i EQUAL ${skip})
        # already added in prior step
        continue()
      endif()
    endif()

    list(APPEND _work "${f}")
  endforeach()

  list(APPEND _flags "${_work}")

else(_Wflags)

  pop_flag("${raw}" -Xlinker _Xflags)
  if(_Xflags)
    set(CMAKE_C_LINKER_WRAPPER_FLAG "-Xlinker" " ")
    set(CMAKE_CXX_LINKER_WRAPPER_FLAG "-Xlinker" " ")
    set(CMAKE_Fortran_LINKER_WRAPPER_FLAG "-Xlinker" " ")
    string(REPLACE ";" "," _Xflags "${_Xflags}")
    list(APPEND _flags "LINKER:${_Xflags}")
  endif()

endif(_Wflags)

set(${outvar} "${_flags}" PARENT_SCOPE)

endfunction(get_link_flags)


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


function(pop_path raw outvar)
# these are file paths without flags like /usr/lib/mpi.so

if(MSVC)
  return()
endif()

set(flag /)
set(_v)
string(REGEX MATCHALL "(^| )${flag} *([^\" ]+|\"[^\"]+\")" _vars "${raw}")
foreach(_p IN LISTS _vars)
  string(REGEX REPLACE "(^| )${flag} *" "" _p "${_p}")
  list(APPEND _v "/${_p}")
endforeach()

# check if compiler absolute path is first element and remove
if("${raw}" MATCHES "^/")
  list(REMOVE_AT _v 0)
endif()

set(${outvar} ${_v} PARENT_SCOPE)

endfunction(pop_path)


function(find_c)

# mpich / openmpi / Intel MPI: mpi
# MS-MPI: msmpi
# Intel Windows: impi

set(MPI_C_LIBRARY)

if(WIN32)
  if(CMAKE_C_COMPILER_ID MATCHES "^Intel")
    set(mpi_libname impi)
  else()
    set(mpi_libname msmpi)
  endif()
else()
  set(mpi_libname mpi)
endif()

if(CMAKE_C_COMPILER_ID MATCHES "^Intel")
  set(wrap_name mpiicc mpiicc.bat)
else()
  set(wrap_name mpicc mpicc.openmpi mpicc.mpich)
endif()

find_program(MPI_C_COMPILER
  NAMES ${wrap_name}
  HINTS ${pc_mpi_c_PREFIX} ${_hints}
  NAMES_PER_DIR
  PATHS ${_binpref}
  PATH_SUFFIXES ${mpi_binsuf}
  )
if(MPI_C_COMPILER)
  cmake_path(GET MPI_C_COMPILER PARENT_PATH mpi_root)
  cmake_path(GET mpi_root PARENT_PATH mpi_root)

  get_flags(${MPI_C_COMPILER} c_raw)
  if(c_raw)
    pop_flag(${c_raw} -I inc_dirs)
    pop_flag(${c_raw} ${CMAKE_LIBRARY_PATH_FLAG} mpi_libdirs)

    pop_path(${c_raw} MPI_C_LIBRARY_fullpath)

    get_link_flags(${c_raw} MPI_C_LINK_FLAGS)
  endif(c_raw)
endif(MPI_C_COMPILER)

find_library(MPI_C_LIBRARY
  NAMES ${mpi_libname}
  HINTS ${mpi_libdirs} ${mpi_root} ${pc_mpi_c_LIBRARY_DIRS} ${pc_mpi_c_LIBDIR} ${_hints}
)

list(APPEND MPI_C_LIBRARY ${MPI_C_LIBRARY_fullpath})

find_path(MPI_C_INCLUDE_DIR
  NAMES mpi.h
  HINTS ${inc_dirs} ${mpi_root} ${pc_mpi_c_INCLUDE_DIRS} ${_hints} ${_hints_inc}
)
if(NOT (MPI_C_LIBRARY AND MPI_C_INCLUDE_DIR))
  return()
endif()

set(CMAKE_REQUIRED_INCLUDES ${MPI_C_INCLUDE_DIR})
set(CMAKE_REQUIRED_LINK_OPTIONS ${MPI_C_LINK_FLAGS})
set(CMAKE_REQUIRED_LIBRARIES ${MPI_C_LIBRARY})
list(APPEND CMAKE_REQUIRED_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})

if(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/find_mpi/get_mpi_version.c)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/find_mpi/get_mpi_version.c
[=[
#include <mpi.h>
#include <stdio.h>

int main(void) {
int version, subversion;

int ierr = MPI_Get_version(&version, &subversion);
if (ierr != 0) return 1;
printf("CMAKE_MPI_VERSION %d.%d\n", version, subversion);

return 0;
}
]=]
)
endif()

if(NOT MPI_VERSION)
  message(CHECK_START "Checking MPI API level")

  try_run(mpi_run_code mpi_build_code
    ${CMAKE_CURRENT_BINARY_DIR}/find_mpi/build
    ${CMAKE_CURRENT_BINARY_DIR}/find_mpi/get_mpi_version.c
    CMAKE_FLAGS -DINCLUDE_DIRECTORIES=${MPI_C_INCLUDE_DIR}
    LINK_OPTIONS ${MPI_C_LINK_FLAGS}
    LINK_LIBRARIES ${MPI_C_LIBRARY}
    RUN_OUTPUT_VARIABLE MPI_VERSION_STRING
    COMPILE_OUTPUT_VARIABLE mpi_vers_build_out
  )

  if(NOT mpi_build_code)
    message(CHECK_FAIL "MPI_VERSION test failed to build:
    ${mpi_vers_build_out}"
    )
    return()
  endif()

  # We don't if(mpi_run_code EQUAL 0) as some platforms / MPI libs don't precisely
  # return 0. The regex should be enough.
  if("${MPI_VERSION_STRING}" MATCHES "CMAKE_MPI_VERSION ([0-9]+\\.[0-9]+)")
    set(MPI_VERSION ${CMAKE_MATCH_1} CACHE STRING "MPI API level")
    message(CHECK_PASS "${MPI_VERSION}")
  endif()


  if(NOT MPI_VERSION)
    message(CHECK_FAIL "MPI API not detected with:
      MPI_C_LIBRARY: ${MPI_C_LIBRARY}
      MPI_C_INCLUDE_DIR: ${MPI_C_INCLUDE_DIR}
      MPI_C_LINK_FLAGS: ${MPI_C_LINK_FLAGS}
      Run output: ${MPI_VERSION_STRING}
      Run return code: ${mpi_run_code}
      "
    )
  endif()
endif()

check_source_compiles(C
[=[
#include <mpi.h>
#ifndef NULL
#define NULL 0
#endif
int main(void) {
    MPI_Init(NULL, NULL);
    MPI_Finalize();
    return 0;
  }
]=]
MPI_C_links)

if(MPI_C_links)
  set(MPI_C_INCLUDE_DIR ${MPI_C_INCLUDE_DIR} PARENT_SCOPE)
  set(MPI_C_LIBRARY "${MPI_C_LIBRARY}" PARENT_SCOPE)
  set(MPI_C_LINK_FLAGS "${MPI_C_LINK_FLAGS}" PARENT_SCOPE)
  set(MPI_C_FOUND true PARENT_SCOPE)
endif()

endfunction(find_c)


function(find_cxx)

# mpich / openmpi / IntelMPI: mpi
# MS-MPI: msmpi
# Intel Windows: impi

set(MPI_CXX_LIBRARY)

if(WIN32)
  if(CMAKE_CXX_COMPILER_ID MATCHES "^Intel")
    set(mpi_libname impi)
  else()
    set(mpi_libname msmpi)
  endif()
else()
  set(mpi_libname mpi_cxx mpi)
endif()

if(NOT (HDF5_ROOT OR DEFINED MPI_CXX_COMPILER))
  pkg_search_module(pc_mpi_cxx ompi-cxx ompi mpich)
endif()

if(CMAKE_CXX_COMPILER_ID MATCHES "^Intel")
  set(wrap_name mpiicpc mpiicpc.bat)
else()
  set(wrap_name mpicxx mpicxx.openmpi mpicxx.mpich)
endif()

find_program(MPI_CXX_COMPILER
  NAMES ${wrap_name}
  HINTS ${pc_mpi_cxx_PREFIX} ${_hints}
  NAMES_PER_DIR
  PATHS ${_binpref}
  PATH_SUFFIXES ${mpi_binsuf}
  )
if(MPI_CXX_COMPILER)
  cmake_path(GET MPI_CXX_COMPILER PARENT_PATH mpi_root)
  cmake_path(GET mpi_root PARENT_PATH mpi_root)

  get_flags(${MPI_CXX_COMPILER} cxx_raw)
  if(cxx_raw)
    pop_flag(${cxx_raw} -I inc_dirs)
    pop_flag(${cxx_raw} ${CMAKE_LIBRARY_PATH_FLAG} mpi_libdirs)

    pop_path(${cxx_raw} MPI_CXX_LIBRARY)

    get_link_flags(${cxx_raw} MPI_CXX_LINK_FLAGS)
  endif(cxx_raw)
endif(MPI_CXX_COMPILER)

foreach(n ${mpi_libname})
  find_library(MPI_CXX_${n}_LIBRARY
    NAMES ${n}
    HINTS ${mpi_libdirs} ${mpi_root} ${pc_mpi_cxx_LIBRARY_DIRS} ${pc_mpi_cxx_LIBDIR} ${_hints}
  )
  if(MPI_CXX_${n}_LIBRARY)
    list(APPEND MPI_CXX_LIBRARY ${MPI_CXX_${n}_LIBRARY})
  endif()
endforeach()

find_path(MPI_CXX_INCLUDE_DIR
  NAMES mpi.h
  HINTS ${inc_dirs} ${mpi_root} ${pc_mpi_cxx_INCLUDE_DIRS} ${_hints} ${_hints_inc}
)
if(NOT (MPI_CXX_LIBRARY AND MPI_CXX_INCLUDE_DIR))
  return()
endif()

set(CMAKE_REQUIRED_INCLUDES ${MPI_CXX_INCLUDE_DIR})
set(CMAKE_REQUIRED_LINK_OPTIONS ${MPI_CXX_LINK_FLAGS})
set(CMAKE_REQUIRED_LIBRARIES ${MPI_CXX_LIBRARY})
list(APPEND CMAKE_REQUIRED_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})

check_source_compiles(CXX
[=[
#include <mpi.h>
#ifndef NULL
#define NULL 0
#endif
int main(void) {
    MPI_Init(NULL, NULL);
    MPI_Finalize();
    return 0;
  }
]=]
MPI_CXX_links)

if(MPI_CXX_links)
  set(MPI_CXX_INCLUDE_DIR ${MPI_CXX_INCLUDE_DIR} PARENT_SCOPE)
  set(MPI_CXX_LIBRARY "${MPI_CXX_LIBRARY}" PARENT_SCOPE)
  set(MPI_CXX_LINK_FLAGS "${MPI_CXX_LINK_FLAGS}" PARENT_SCOPE)
  set(MPI_CXX_FOUND true PARENT_SCOPE)
endif()

endfunction(find_cxx)


function(find_fortran)

# mpich / openmpi / Intel MPI: mpi
# MS-MPI: msmpi
# Intel Windows: impi

set(MPI_Fortran_LIBRARY)

if(WIN32)
  if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
    set(mpi_libname impi)
  else()
    set(mpi_libname msmpi)
  endif()
elseif(DEFINED ENV{I_MPI_ROOT})
  set(mpi_libname mpi)
else()
  set(mpi_libname
  mpi_usempif08 mpi_usempi_ignore_tkr mpi_mpifh
  mpifort
  mpi)
endif()

if(NOT (HDF5_ROOT OR DEFINED MPI_Fortran_COMPILER))
  pkg_search_module(pc_mpi_f ompi-fort ompi mpich)
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set(wrap_name mpiifort mpiifort.bat)
else()
  set(wrap_name mpifort mpifc mpifort.openmpi mpifort.mpich)
endif()

find_program(MPI_Fortran_COMPILER
  NAMES ${wrap_name}
  HINTS ${pc_mpi_f_PREFIX} ${_hints}
  NAMES_PER_DIR
  PATHS ${_binpref}
  PATH_SUFFIXES ${mpi_binsuf}
  )
if(MPI_Fortran_COMPILER)
  cmake_path(GET MPI_Fortran_COMPILER PARENT_PATH mpi_root)
  cmake_path(GET mpi_root PARENT_PATH mpi_root)

  get_flags(${MPI_Fortran_COMPILER} f_raw)
  if(f_raw)
    pop_flag(${f_raw} -I inc_dirs)
    pop_flag(${f_raw} ${CMAKE_LIBRARY_PATH_FLAG} mpi_libdirs)

    pop_path(${f_raw} MPI_Fortran_LIBRARY)

    get_link_flags(${f_raw} MPI_Fortran_LINK_FLAGS)
  endif(f_raw)
endif(MPI_Fortran_COMPILER)

foreach(n ${mpi_libname})
  find_library(MPI_Fortran_${n}_LIBRARY
    NAMES ${n}
    HINTS ${mpi_libdirs} ${mpi_root} ${pc_mpi_f_LIBRARY_DIRS} ${pc_mpi_f_LIBDIR} ${_hints}
  )
  if(MPI_Fortran_${n}_LIBRARY)
    list(APPEND MPI_Fortran_LIBRARY ${MPI_Fortran_${n}_LIBRARY})
  endif()
endforeach()

find_path(MPI_Fortran_INCLUDE_DIR
  NAMES mpi.mod
  HINTS ${inc_dirs} ${mpi_root} ${pc_mpi_f_INCLUDE_DIRS} ${_hints} ${_hints_inc}
  PATH_SUFFIXES lib
  # openmpi puts .mod files into lib/
)

if(WIN32 AND NOT CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  find_path(MPI_Fortran_INCLUDE_EXTRA
    NAMES mpifptr.h
    HINTS ${inc_dirs} ${mpi_root} ${pc_mpi_f_INCLUDE_DIRS} ${_hints} ${_hints_inc}
    PATH_SUFFIXES x64
  )

  if(MPI_Fortran_INCLUDE_EXTRA AND NOT MPI_Fortran_INCLUDE_EXTRA STREQUAL ${MPI_Fortran_INCLUDE_DIR})
    list(APPEND MPI_Fortran_INCLUDE_DIR ${MPI_Fortran_INCLUDE_EXTRA})
  endif()
endif()

if(NOT (MPI_Fortran_LIBRARY AND MPI_Fortran_INCLUDE_DIR))
  return()
endif()

set(CMAKE_REQUIRED_INCLUDES ${MPI_Fortran_INCLUDE_DIR})
set(CMAKE_REQUIRED_LINK_OPTIONS ${MPI_Fortran_LINK_FLAGS})
set(CMAKE_REQUIRED_LIBRARIES ${MPI_Fortran_LIBRARY})
list(APPEND CMAKE_REQUIRED_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})

check_source_compiles(Fortran
[=[
program test
use mpi
implicit none
integer :: i
call mpi_init(i)
call mpi_finalize(i)
end program
]=]
MPI_Fortran_links)

check_source_compiles(Fortran
[=[
program test
use mpi_f08, only : mpi_comm_rank, mpi_comm_world, mpi_init, mpi_finalize
implicit none
call mpi_init
call mpi_finalize
end program
]=]
MPI_Fortran_HAVE_F08_MODULE)

if(MPI_Fortran_links)
  set(MPI_Fortran_INCLUDE_DIR ${MPI_Fortran_INCLUDE_DIR} PARENT_SCOPE)
  set(MPI_Fortran_LIBRARY "${MPI_Fortran_LIBRARY}" PARENT_SCOPE)
  set(MPI_Fortran_LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}" PARENT_SCOPE)
  set(MPI_Fortran_HAVE_F90_MODULE true PARENT_SCOPE)
  set(MPI_Fortran_HAVE_F08_MODULE ${MPI_Fortran_HAVE_F08_MODULE} PARENT_SCOPE)
  set(MPI_Fortran_FOUND true PARENT_SCOPE)
endif()

endfunction(find_fortran)

#===== main program ======

set(_hints)
set(_hints_inc)

find_package(PkgConfig)
find_package(Threads)

if(NOT MPI_ROOT AND DEFINED ENV{MPI_ROOT})
  set(MPI_ROOT $ENV{MPI_ROOT})
endif()

# Intel MPI, which works with non-Intel compilers on Linux
if((CMAKE_SYSTEM_NAME STREQUAL Linux OR CMAKE_C_COMPILER_ID MATCHES "^Intel") AND
      DEFINED ENV{I_MPI_ROOT})
  set(_hints $ENV{I_MPI_ROOT})
endif()

if(WIN32 AND NOT CMAKE_C_COMPILER_ID MATCHES "^Intel")
  set(_hints $ENV{MSMPI_LIB64})
  set(_hints_inc $ENV{MSMPI_INC})
endif()

set(mpi_binsuf)
if(NOT HDF5_ROOT AND NOT DEFINED ENV{I_MPI_ROOT})
  set(mpi_binsuf bin openmpi/bin mpich/bin)
endif()

if(UNIX)
  set(_binpref /usr/lib64)
else()
  set(_binpref $ENV{MINGWROOT} $ENV{MSMPI_BIN})
endif()

if(NOT (HDF5_ROOT OR DEFINED MPI_C_COMPILER))
  pkg_search_module(pc_mpi_c ompi-c ompi mpich)
endif()

# must have MPIexec to be worthwhile (de facto standard is mpiexec)
find_program(MPIEXEC_EXECUTABLE
  NAMES mpiexec
  HINTS ${pc_mpi_c_PREFIX} ${_hints}
  PATHS ${_binpref}
  PATH_SUFFIXES ${mpi_binsuf}
)

# like factory FindMPI, always find MPI_C
find_c()

if(CXX IN_LIST MPI_FIND_COMPONENTS)
  find_cxx()
endif()

if(Fortran IN_LIST MPI_FIND_COMPONENTS)
  find_fortran()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MPI
  REQUIRED_VARS MPIEXEC_EXECUTABLE
  VERSION_VAR MPI_VERSION
  HANDLE_COMPONENTS)

if(MPI_C_FOUND)
  set(MPI_C_LIBRARIES ${MPI_C_LIBRARY})
  set(MPI_C_INCLUDE_DIRS ${MPI_C_INCLUDE_DIR})
  if(NOT TARGET MPI::MPI_C)
    add_library(MPI::MPI_C IMPORTED INTERFACE)
    set_target_properties(MPI::MPI_C PROPERTIES
      INTERFACE_LINK_LIBRARIES "${MPI_C_LIBRARIES}"
      INTERFACE_INCLUDE_DIRECTORIES "${MPI_C_INCLUDE_DIRS}"
    )
    if(MPI_C_LINK_FLAGS)
      set_target_properties(MPI::MPI_C PROPERTIES INTERFACE_LINK_OPTIONS "${MPI_C_LINK_FLAGS}")
    endif()
  endif()
endif(MPI_C_FOUND)

if(MPI_CXX_FOUND)
  set(MPI_CXX_LIBRARIES ${MPI_CXX_LIBRARY})
  set(MPI_CXX_INCLUDE_DIRS ${MPI_CXX_INCLUDE_DIR})
  if(NOT TARGET MPI::MPI_CXX)
    add_library(MPI::MPI_CXX IMPORTED INTERFACE)
    set_target_properties(MPI::MPI_CXX PROPERTIES
      INTERFACE_LINK_LIBRARIES "${MPI_CXX_LIBRARIES}"
      INTERFACE_INCLUDE_DIRECTORIES "${MPI_CXX_INCLUDE_DIRS}"
    )
    if(MPI_CXX_LINK_FLAGS)
      set_target_properties(MPI::MPI_CXX PROPERTIES INTERFACE_LINK_OPTIONS "${MPI_CXX_LINK_FLAGS}")
    endif()
  endif()
endif(MPI_CXX_FOUND)

if(MPI_Fortran_FOUND)
  set(MPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARY})
  set(MPI_Fortran_INCLUDE_DIRS ${MPI_Fortran_INCLUDE_DIR})
  if(NOT TARGET MPI::MPI_Fortran)
    add_library(MPI::MPI_Fortran IMPORTED INTERFACE)
    set_target_properties(MPI::MPI_Fortran PROPERTIES
      INTERFACE_LINK_LIBRARIES "${MPI_Fortran_LIBRARIES}"
      INTERFACE_INCLUDE_DIRECTORIES "${MPI_Fortran_INCLUDE_DIRS}"
    )
    if(MPI_Fortran_LINK_FLAGS)
      set_target_properties(MPI::MPI_Fortran PROPERTIES INTERFACE_LINK_OPTIONS "${MPI_Fortran_LINK_FLAGS}")
    endif()
  endif()

endif(MPI_Fortran_FOUND)

if(MPI_FOUND)
  set(MPI_LIBRARIES ${MPI_Fortran_LIBRARIES} ${MPI_C_LIBRARIES})
  set(MPI_INCLUDE_DIRS ${MPI_Fortran_INCLUDE_DIRS} ${MPI_C_INCLUDE_DIRS})

  set(MPIEXEC_NUMPROC_FLAG "-n"  CACHE STRING "Flag used by MPI to specify the number of processes for mpiexec; the next option will be the number of processes.")
  cmake_host_system_information(RESULT _n QUERY NUMBER_OF_PHYSICAL_CORES)
  set(MPIEXEC_MAX_NUMPROCS "${_n}" CACHE STRING "Maximum number of processors available to run MPI applications.")

  message(VERBOSE "FindMPI results:
    MPI_C_COMPILER: ${MPI_C_COMPILER}
    MPI_C_LIBRARIES: ${MPI_C_LIBRARIES}
    MPI_C_INCLUDE_DIRS: ${MPI_C_INCLUDE_DIRS}
    MPI_C_LINK_FLAGS: ${MPI_C_LINK_FLAGS}

    MPI_Fortran_COMPILER: ${MPI_Fortran_COMPILER}
    MPI_Fortran_LIBRARIES: ${MPI_Fortran_LIBRARIES}
    MPI_Fortran_INCLUDE_DIRS: ${MPI_Fortran_INCLUDE_DIRS}
    MPI_Fortran_LINK_FLAGS: ${MPI_Fortran_LINK_FLAGS}

    MPIEXEC_EXECUTABLE: ${MPIEXEC_EXECUTABLE}
    MPIEXEC_MAX_NUMPROCS: ${MPIEXEC_MAX_NUMPROCS}
    MPI_VERSION: ${MPI_VERSION}
    ")
endif()

mark_as_advanced(
MPI_Fortran_LIBRARY MPI_Fortran_INCLUDE_DIR MPI_Fortran_HAVE_F90_MODULE MPI_Fortran_HAVE_F08_MODULE
MPI_C_LIBRARY MPI_C_INCLUDE_DIR
MPIEXEC_EXECUTABLE MPIEXEC_NUMPROC_FLAG MPIEXEC_MAX_NUMPROCS
)
