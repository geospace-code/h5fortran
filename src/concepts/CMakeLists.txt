add_executable(compact_dataset h5compact.f90)
target_link_libraries(compact_dataset PRIVATE HDF5::HDF5)
add_test(NAME proto:compact COMMAND $<TARGET_FILE:compact_dataset>)

add_executable(int64_write_read int64.f90)
target_link_libraries(int64_write_read PRIVATE HDF5::HDF5)
add_test(NAME proto:int64_write_read COMMAND $<TARGET_FILE:int64_write_read>)


add_executable(proto_file_image file_image.f90)
target_link_libraries(proto_file_image PRIVATE HDF5::HDF5)
add_test(NAME proto:file_image COMMAND $<TARGET_FILE:proto_file_image>)
