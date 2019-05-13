add_library(hdf5oo hdf5_interface.f90
  read.f90 read_real64.f90 read_real32.f90 read_int.f90
  write.f90 write_real64.f90 write_real32.f90 write_int.f90
  string_utils.f90)
# need BOTH includes, for some systems!
target_include_directories(hdf5oo PRIVATE ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(hdf5oo PRIVATE ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
target_compile_options(hdf5oo PRIVATE ${HDF5_Fortran_DEFINITIONS})
set_target_properties(hdf5oo PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})
