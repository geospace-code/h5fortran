# 3.12: Fortran submodule
cmake_minimum_required (VERSION 3.12)
project(hdf5iface Fortran)
enable_testing()

include(cmake/compilers.cmake)

include(cmake/hdf5.cmake)

add_library(hdf5oo src/hdf5_interface.f90 
  src/read.f90 src/read_real64.f90 src/read_real32.f90 src/read_int.f90
  src/write.f90 src/write_real64.f90 src/write_real32.f90 src/write_int.f90)
# need BOTH includes, for some systems!
target_include_directories(hdf5oo PRIVATE ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(hdf5oo PRIVATE ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
target_compile_options(hdf5oo PRIVATE ${HDF5_Fortran_DEFINITIONS})

add_executable(testh5 src/test_hdf5_ifc.f90)
target_link_libraries(testh5 PRIVATE hdf5oo)
target_compile_options(testh5 PRIVATE -Wno-compare-reals)
add_test(NAME h5oo COMMAND testh5)
set_tests_properties(h5oo PROPERTIES FIXTURES_SETUP h5files)

add_executable(shapes_check src/GetShape.f90)
target_link_libraries(shapes_check PRIVATE hdf5oo)
add_test(NAME ShapeCheck COMMAND shapes_check p5.h5 group69/flux_node)
set_tests_properties(ShapeCheck PROPERTIES FIXTURES_REQUIRED h5files)
