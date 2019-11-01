add_library(h5fortran hdf5_interface.f90
  read.f90 read_real64.f90 read_real32.f90 read_int.f90
  write.f90 write_real64.f90 write_real32.f90 write_int.f90
  string_utils.f90)
# need BOTH includes, for some systems!
target_include_directories(h5fortran PRIVATE ${HDF5_INCLUDE_DIRS} ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(h5fortran PRIVATE ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES})
target_compile_options(h5fortran PRIVATE ${HDF5_Fortran_DEFINITIONS})
set_target_properties(h5fortran PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}
  ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
