# don't enclose this all in "if(NOT DEFINED HDF5OK)" because CMake intermittantly doesn't cache needed HDF5 variables.

set(HDF5_USE_STATIC_LIBRARIES true)
# Intel HDF5 for Windows has some real issues from the factory, this makes it work:
if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(HDF5_NO_FIND_PACKAGE_CONFIG_FILE true)
  set(HDF5_USE_STATIC_LIBRARIES false)
endif()

find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
list(APPEND HDF5_INCLUDE_DIRS ${HDF5_Fortran_INCLUDE_DIRS})
# this is because HDF5's CONFIG files are incorrect (at least for 1.10.5)
find_path(HDF5_MODULE_DIR NAMES hdf5.mod HINTS ${HDF5_INCLUDE_DIRS} PATH_SUFFIXES static)
if(HDF5_MODULE_DIR)
  list(APPEND HDF5_INCLUDE_DIRS ${HDF5_MODULE_DIR})
  set(SZIP_ROOT "${HDF5_MODULE_DIR}/..;${HDF5_MODULE_DIR}/../..")
  set(ZLIB_ROOT ${SZIP_ROOT})
endif()
list(REMOVE_DUPLICATES HDF5_INCLUDE_DIRS)

# in case the compiler wrapper didn't work
set(HDF5_LIBRARIES ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES} ${HDF5_LIBRARIES})

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

if(DEFINED HDF5OK)
  return()
endif()

message(STATUS "HDF5 include: ${HDF5_INCLUDE_DIRS}")
message(STATUS "HDF5 library: ${HDF5_LIBRARIES}")
# we don't use these because they may come from a broken compiler wrapper
if(HDF5_Fortran_COMPILER_EXECUTABLE)
  message(STATUS "HDF5 Fortran compiler: ${HDF5_Fortran_COMPILER_EXECUTABLE}")
endif()
if(HDF5_Fortran_DEFINITIONS)
  message(STATUS "HDF5 compiler defs: ${HDF5_Fortran_DEFINITIONS}")
endif()

set(CMAKE_REQUIRED_INCLUDES ${HDF5_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${HDF5_LIBRARIES})

include(CheckFortranSourceCompiles)
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/src/tests/test_minimal.f90 _code)
check_fortran_source_compiles(${_code} HDF5OK SRC_EXT f90)

if(NOT HDF5OK)
  message(FATAL_ERROR "HDF5 library not working with ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
endif(NOT HDF5OK)
