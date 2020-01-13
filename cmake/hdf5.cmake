# don't enclose this all in "if(NOT DEFINED HDF5OK)" because CMake intermittantly doesn't cache needed HDF5 variables.

set(HDF5_SEARCH_WRAPPER true)
set(HDF5_USE_STATIC_LIBRARIES true)

# Intel HDF5 for Windows has some real issues from the factory, this makes it work:
if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(HDF5_NO_FIND_PACKAGE_CONFIG_FILE true)
  set(HDF5_SEARCH_WRAPPER false)
  set(HDF5_USE_STATIC_LIBRARIES false)
endif()

# set(HDF5_FIND_DEBUG true)

find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
if(MINGW)
  set(HDF5_INCLUDE_DIRS ${HDF5_INCLUDE_DIRS}/static)
  message(${HDF5_INCLUDE_DIRS})
endif()

set(HDF5_LIBRARIES ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES} ${HDF5_LIBRARIES})
list(APPEND HDF5_INCLUDE_DIRS ${HDF5_Fortran_INCLUDE_DIRS})

if(DEFINED HDF5OK)
  return()
endif()

message(STATUS "HDF5 include: ${HDF5_INCLUDE_DIRS}")
message(STATUS "HDF5 library: ${HDF5_LIBRARIES}")
if(HDF5_Fortran_COMPILER_EXECUTABLE)
  message(STATUS "HDF5 Fortran compiler: ${HDF5_Fortran_COMPILER_EXECUTABLE}")
endif()
if(HDF5_Fortran_DEFINITIONS)
  message(STATUS "HDF5 compiler defs: ${HDF5_Fortran_DEFINITIONS}")
endif()

set(CMAKE_REQUIRED_INCLUDES ${HDF5_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${HDF5_LIBRARIES})

include(CheckFortranSourceCompiles)
file(READ ${CMAKE_SOURCE_DIR}/src/tests/test_minimal.f90 _code)
check_fortran_source_compiles(${_code} HDF5OK SRC_EXT f90)

if(NOT HDF5OK)
  message(FATAL_ERROR "HDF5 library not working with ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
endif(NOT HDF5OK)
