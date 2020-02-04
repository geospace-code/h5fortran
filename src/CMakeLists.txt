add_library(h5fortran hdf5_interface.f90
  read.f90 reader.f90
  write.f90 writer.f90
  string_utils.f90)
target_include_directories(h5fortran PRIVATE ${HDF5_INCLUDE_DIRS}
  INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
target_link_libraries(h5fortran PRIVATE ${HDF5_LIBRARIES})
target_compile_options(h5fortran PRIVATE ${HDF5_Fortran_DEFINITIONS})
set_target_properties(h5fortran PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
  ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
add_library(h5fortran::h5fortran ALIAS h5fortran)
