# real32, real64 supported by HDF5 1.10 https://support.hdfgroup.org/HDF5/doc/RM/PredefDTypes.html
cmake_minimum_required (VERSION 3.10)
project(hdf5iface Fortran)
enable_testing()

include(compilers.cmake)

find_package(HDF5 REQUIRED COMPONENTS Fortran Fortran_HL)

set(CMAKE_REQUIRED_INCLUDES ${HDF5_Fortran_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
include(CheckFortranSourceCompiles)
check_fortran_source_compiles("program nc; use h5lt; end" hasHDF5
                              SRC_EXT f90)
if(NOT hasHDF5)
  message(FATAL_ERROR "HDF5 library not working with " ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION})
endif()

add_library(hdf5oo hdf5_interface.f90)
# need BOTH includes, for some older systems!
target_include_directories(hdf5oo PRIVATE ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(hdf5oo PRIVATE ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
target_compile_options(hdf5oo PRIVATE ${HDF5_Fortran_DEFINITIONS})

add_executable(testh5 test_hdf5_ifc.f90)
target_link_libraries(testh5 PRIVATE hdf5oo)
target_compile_options(testh5 PRIVATE -Wno-compare-reals)
add_test(NAME h5oo COMMAND testh5)
