target_sources(h5fortran PRIVATE
interface.f90
read.f90 reader.f90 reader_lt.f90
write.f90 writer.f90 writer_lt.f90
string_utils.f90)


if(prototype)
  add_executable(proto_file_image file_image.f90)
  target_include_directories(proto_file_image
    PRIVATE ${HDF5_INCLUDE_DIRS}
    PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include)  # for IBM XL
  target_link_libraries(proto_file_image PRIVATE ${HDF5_LIBRARIES})
  set_target_properties(proto_file_image PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
  add_test(NAME proto:file_image COMMAND $<TARGET_FILE:proto_file_image>)
endif()
