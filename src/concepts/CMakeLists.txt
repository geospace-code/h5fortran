
add_executable(proto_file_image file_image.f90)
target_include_directories(proto_file_image
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include)  # for IBM XL
target_link_libraries(proto_file_image PRIVATE HDF5::HDF5)
set_target_properties(proto_file_image PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
add_test(NAME proto:file_image COMMAND $<TARGET_FILE:proto_file_image>)
