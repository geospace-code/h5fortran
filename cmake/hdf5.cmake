find_package(HDF5 REQUIRED COMPONENTS Fortran Fortran_HL)

set(CMAKE_REQUIRED_INCLUDES ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
check_fortran_source_compiles("
program a
use h5lt
end" 
  hasHDF5
  SRC_EXT f90)

if(NOT hasHDF5)
  message(FATAL_ERROR "HDF5 library not working with ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
endif()
