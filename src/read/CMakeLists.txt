file(READ reader_lt_template.in.f90 reader_lt_template)
configure_file(reader_lt.in.f90 reader_lt.f90 @ONLY)

target_sources(h5fortran PRIVATE
  read.f90 reader.f90
  ${CMAKE_CURRENT_BINARY_DIR}/reader_lt.f90)
