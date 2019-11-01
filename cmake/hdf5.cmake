set(HDF5_USE_STATIC_LIBRARIES ON)

find_package(HDF5 REQUIRED COMPONENTS Fortran Fortran_HL)

if(WIN32)
  # Needed for MSYS2, this directory wasn't in CMake 3.15.2 FindHDF5
  if(EXISTS ${HDF5_INCLUDE_DIRS}/static)
    list(APPEND HDF5_INCLUDE_DIRS ${HDF5_INCLUDE_DIRS}/static)
  endif()
endif()

message(STATUS "HDF5 includes: ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS}")
message(STATUS "HDF5 library: ${HDF5_Fortran_LIBRARIES}   H5LT library: ${HDF5_Fortran_HL_LIBRARIES}")
if(HDF5_Fortran_COMPILER_EXECUTABLE)
  message(STATUS "HDF5 Fortran compiler: ${HDF5_Fortran_COMPILER_EXECUTABLE}")
endif()
if(HDF5_Fortran_DEFINITIONS)
  message(STATUS "HDF5 compiler defs: ${HDF5_Fortran_DEFINITIONS}")
endif()

set(CMAKE_REQUIRED_INCLUDES ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES})

include(CheckFortranSourceCompiles)
check_fortran_source_compiles("use h5lt; end" hasHDF5 SRC_EXT f90)

if(NOT hasHDF5)
  message(WARNING "HDF5 library may not be working with ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
endif()
