# real32, real64 supported by HDF5 1.10 https://support.hdfgroup.org/HDF5/doc/RM/PredefDTypes.html
cmake_minimum_required (VERSION 2.8.12)
project(hdf5iface Fortran)

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
    list(APPEND FFLAGS -check all -traceback -warn -debug extended)
    set(F64 -r8)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
    list(APPEND FFLAGS -march=native -Wall -Wextra -Wpedantic -Werror=array-bounds -fbacktrace -fcheck=all)
    set(F64 -fdefault-real-8)
endif()

add_compile_options(-O3 -g ${FFLAGS})
#------------------------
find_package(HDF5 REQUIRED COMPONENTS Fortran Fortran_HL HL)
include_directories(${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})


add_library(hdf5iface_32 hdf5_interface.f90) 
target_link_libraries(hdf5iface_32
    ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARY_dl})
    
add_library(hdf5iface_64 hdf5_interface.f90) 
target_compile_options(hdf5iface_64 PUBLIC ${F64})
target_link_libraries(hdf5iface_64
    ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARY_dl})

add_executable(test_hdf5_ifc_64 test_hdf5_ifc.f90)
target_link_libraries(test_hdf5_ifc_64 hdf5iface_64)
target_compile_options(test_hdf5_ifc_64 PUBLIC ${F64})

add_executable(test_hdf5_ifc_32 test_hdf5_ifc.f90)
target_link_libraries(test_hdf5_ifc_32 hdf5iface_32)

#---------------------------
enable_testing()
add_test(NAME h5iface_32 COMMAND test_hdf5_ifc_32)

add_test(NAME h5iface_64 COMMAND test_hdf5_ifc_64)
