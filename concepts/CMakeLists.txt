cmake_minimum_required(VERSION 3.20)

project(HDF5concepts
LANGUAGES C Fortran
)

set_directory_properties(PROPERTIES LABELS concept)

cmake_path(SET CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules)

find_package(HDF5 COMPONENTS Fortran REQUIRED)

add_executable(compact_dataset h5compact.f90)
target_link_libraries(compact_dataset PRIVATE HDF5::HDF5)
add_test(NAME compact COMMAND compact_dataset)

add_executable(int64_write_read int64.f90)
target_link_libraries(int64_write_read PRIVATE HDF5::HDF5)
add_test(NAME int64_write_read COMMAND int64_write_read)

add_executable(proto_file_image file_image.f90)
target_link_libraries(proto_file_image PRIVATE HDF5::HDF5)
add_test(NAME file_image COMMAND proto_file_image)

add_executable(virtual_dataset virtual_dataset.f90)
target_link_libraries(virtual_dataset PRIVATE HDF5::HDF5)
add_test(NAME virtual_dataset COMMAND virtual_dataset)
