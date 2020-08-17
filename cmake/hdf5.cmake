# don't enclose this all in "if(NOT DEFINED HDF5OK)" because CMake intermittantly doesn't cache needed HDF5 variables.

set(HDF5_USE_STATIC_LIBRARIES true)
# Intel HDF5 for Windows has some real issues from the factory, this makes it work:
if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(HDF5_NO_FIND_PACKAGE_CONFIG_FILE true)
  set(HDF5_USE_STATIC_LIBRARIES false)
endif()

if(PROJECT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  find_package(HDF5 COMPONENTS Fortran HL REQUIRED)
else()
  find_package(HDF5 COMPONENTS Fortran HL)
endif()

if(NOT HDF5_FOUND)
  return()
endif()

if(HDF5_Fortran_INCLUDE_DIRS)
  list(APPEND HDF5_INCLUDE_DIRS ${HDF5_Fortran_INCLUDE_DIRS})
endif()
# this is because HDF5's CONFIG files are incorrect (at least for 1.10.5)
find_path(HDF5_MODULE_DIR NAMES hdf5.mod HINTS ${HDF5_INCLUDE_DIRS} PATH_SUFFIXES static)
if(HDF5_MODULE_DIR)
  list(APPEND HDF5_INCLUDE_DIRS ${HDF5_MODULE_DIR})
  set(SZIP_ROOT "${HDF5_MODULE_DIR}/..;${HDF5_MODULE_DIR}/../..")
  set(ZLIB_ROOT "${HDF5_MODULE_DIR}/..;${HDF5_MODULE_DIR}/../..")
endif()
list(REMOVE_DUPLICATES HDF5_INCLUDE_DIRS)

# in case the compiler wrapper didn't work
set(HDF5_LIBRARIES ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES} ${HDF5_LIBRARIES})

# Szip even though not directly used because if system static links libhdf5 with szip,
# our builds will fail if we don't also link szip.
find_package(SZIP)
if(SZIP_FOUND)
  list(APPEND HDF5_LIBRARIES SZIP::SZIP)
endif()

find_package(ZLIB)
if(ZLIB_FOUND)
  list(APPEND HDF5_LIBRARIES ZLIB::ZLIB)
endif()

set(THREADS_PREFER_PTHREAD_FLAG true)
find_package(Threads)
if(Threads_FOUND)
  list(APPEND HDF5_LIBRARIES Threads::Threads)
endif()

list(APPEND HDF5_LIBRARIES ${CMAKE_DL_LIBS})

if(UNIX)
  list(APPEND HDF5_LIBRARIES m)
endif()

if(MSVC)
  # this stanza must be BEFORE if(DEFINED HDF5OK)
  # this is specifically for Intel compiler with HDF5 1.10 or 1.12 binary install.
  if(NOT DEFINED HDF5_ROOT AND DEFINED ENV{HDF5_ROOT})
    file(TO_CMAKE_PATH "$ENV{HDF5_ROOT}" HDF5_ROOT)
  endif()

  if(DEFINED HDF5_ROOT)
    set(ENV{PATH} "${HDF5_ROOT}/bin;$ENV{PATH}")
  endif()
endif()

if(DEFINED HDF5OK)
  return()
endif()

message(STATUS "HDF5 include: ${HDF5_INCLUDE_DIRS}")
message(STATUS "HDF5 library: ${HDF5_LIBRARIES}")
# we don't use these because they may come from a broken compiler wrapper
# if(HDF5_Fortran_COMPILER_EXECUTABLE)
#   message(STATUS "HDF5 Fortran compiler: ${HDF5_Fortran_COMPILER_EXECUTABLE}")
# endif()
# if(HDF5_Fortran_DEFINITIONS)
#   message(STATUS "HDF5 compiler defs: ${HDF5_Fortran_DEFINITIONS}")
# endif()

set(CMAKE_REQUIRED_INCLUDES ${HDF5_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${HDF5_LIBRARIES})

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
check_fortran_source_compiles(${_code} HDF5_compiles_ok SRC_EXT f90)

include(CheckFortranSourceRuns)
check_fortran_source_runs(${_code} HDF5_runs_ok SRC_EXT f90)

set(HDF5OK false)
if(HDF5_compiles_ok AND HDF5_runs_ok)
  set(HDF5OK true CACHE BOOL "HDF5 library compiles and run OK" FORCE)
endif()
