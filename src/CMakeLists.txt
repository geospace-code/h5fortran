if(concepts)
add_subdirectory(concepts)
endif(concepts)

if(WIN32)
  set(is_windows .true.)
else()
  set(is_windows .false.)
endif()
configure_file(pathlib.in.f90 pathlib.f90 @ONLY)

file(READ reader_template.in.f90 reader_template)
configure_file(reader.in.f90 reader.f90 @ONLY)

file(READ reader_lt_template.in.f90 reader_lt_template)
configure_file(reader_lt.in.f90 reader_lt.f90 @ONLY)

file(READ writer_template.in.f90 writer_template)
configure_file(writer.in.f90 writer.f90 @ONLY)

file(READ writer_lt_template.in.f90 writer_lt_template)
configure_file(writer_lt.in.f90 writer_lt.f90 @ONLY)

target_sources(h5fortran PRIVATE
interface.f90
read.f90 ${CMAKE_CURRENT_BINARY_DIR}/reader.f90 ${CMAKE_CURRENT_BINARY_DIR}/reader_lt.f90
write.f90 ${CMAKE_CURRENT_BINARY_DIR}/writer.f90 ${CMAKE_CURRENT_BINARY_DIR}/writer_lt.f90
attributes.f90
${CMAKE_CURRENT_BINARY_DIR}/pathlib.f90)
