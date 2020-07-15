
if(WIN32)
  set(is_windows .true.)
else()
  set(is_windows .false.)
endif()
configure_file(pathlib.in.f90 pathlib.f90)

target_sources(h5fortran PRIVATE
interface.f90
read.f90 reader.f90 reader_lt.f90
write.f90 writer.f90 writer_lt.f90
string_utils.f90 ${CMAKE_CURRENT_BINARY_DIR}/pathlib.f90)
