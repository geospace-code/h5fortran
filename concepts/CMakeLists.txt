cmake_minimum_required(VERSION 3.20)

project(HDF5concepts
LANGUAGES C Fortran
)

cmake_path(SET CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules)

find_package(HDF5 COMPONENTS Fortran REQUIRED)

add_executable(compact_dataset h5compact.f90)
target_link_libraries(compact_dataset PRIVATE HDF5::HDF5)
add_test(NAME proto:compact COMMAND compact_dataset)
set_tests_properties(proto:compact PROPERTIES LABELS unit)

add_executable(int64_write_read int64.f90)
target_link_libraries(int64_write_read PRIVATE HDF5::HDF5)
add_test(NAME proto:int64_write_read COMMAND int64_write_read)
set_tests_properties(proto:int64_write_read PROPERTIES LABELS unit)

add_executable(proto_file_image file_image.f90)
target_link_libraries(proto_file_image PRIVATE HDF5::HDF5)
add_test(NAME proto:file_image COMMAND proto_file_image)
set_tests_properties(proto:file_image PROPERTIES LABELS unit)
