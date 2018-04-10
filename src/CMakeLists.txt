# real32, real64 supported by HDF5 1.10 https://support.hdfgroup.org/HDF5/doc/RM/PredefDTypes.html
cmake_minimum_required (VERSION 3.2.2)
project(hdf5iface Fortran)

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
    list(APPEND FFLAGS -check all -traceback -warn -debug extended)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU) 
    if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 5.4.1)
      message(ERROR " gfortran >= 5.4.1 required, you have " ${CMAKE_Fortran_COMPILER_VERSION})
    endif()
    list(APPEND FFLAGS -std=f2008ts -march=native -Wall -Wextra -Wpedantic -Werror=array-bounds -fbacktrace -fcheck=all)
endif()

add_compile_options(-O3 -g ${FFLAGS})
#------------------------
find_package(HDF5 REQUIRED COMPONENTS Fortran Fortran_HL)

# need BOTH includes, for some older systems!
include_directories(${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})


add_library(hdf5oo hdf5_interface.f90)
target_link_libraries(hdf5oo ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})

# ========== test

add_executable(testh5 test_hdf5_ifc.f90)
target_link_libraries(testh5 hdf5oo)
target_compile_options(testh5 PRIVATE -Wno-compare-reals)

#---------------------------
enable_testing()
add_test(NAME h5oo COMMAND testh5)
