# real32, real64 supported by HDF5 1.10 https://support.hdfgroup.org/HDF5/doc/RM/PredefDTypes.html
cmake_minimum_required (VERSION 3.2.2)
project(hdf5iface Fortran)

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
    list(APPEND FFLAGS -check all -traceback -warn -debug extended)
    set(F64 -r8)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)  # gfortran >= 6
    list(APPEND FFLAGS -march=native -Wall -Wextra -Wpedantic -Werror=array-bounds -fbacktrace -fcheck=all)
    set(F64 -fdefault-real-8)
endif()

add_compile_options(-O3 -g ${FFLAGS})
#------------------------
find_package(HDF5 REQUIRED COMPONENTS Fortran Fortran_HL)
include_directories(${HDF5_Fortran_INCLUDE_DIRS})


add_library(hdf5iface_32 hdf5_interface.f90)
target_link_libraries(hdf5iface_32
    ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
set_target_properties(hdf5iface_32 PROPERTIES Fortran_MODULE_DIRECTORY mod.real32)

add_library(hdf5iface_64 hdf5_interface.f90)
target_compile_options(hdf5iface_64 PUBLIC ${F64})
target_link_libraries(hdf5iface_64
    ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
set_target_properties(hdf5iface_64 PROPERTIES Fortran_MODULE_DIRECTORY mod.real64)


add_executable(test_hdf5_ifc_32 test_hdf5_ifc.f90)
target_link_libraries(test_hdf5_ifc_32 hdf5iface_32)
target_compile_options(test_hdf5_ifc_32 PRIVATE -Wno-compare-reals)
set_target_properties(test_hdf5_ifc_32 PROPERTIES Fortran_MODULE_DIRECTORY mod.real32)

add_executable(test_hdf5_ifc_64 test_hdf5_ifc.f90)
target_link_libraries(test_hdf5_ifc_64 hdf5iface_64)
target_compile_options(test_hdf5_ifc_64 PRIVATE -Wno-compare-reals)
set_target_properties(test_hdf5_ifc_64 PROPERTIES Fortran_MODULE_DIRECTORY mod.real64)
#---------------------------
enable_testing()
add_test(NAME h5iface_32 COMMAND test_hdf5_ifc_32)

add_test(NAME h5iface_64 COMMAND test_hdf5_ifc_64)
