file(READ writer_template_r32.in.f90 writer_template_r32)
file(READ writer_template_r64.in.f90 writer_template_r64)
file(READ writer_template_i32.in.f90 writer_template_i32)
file(READ writer_template_i64.in.f90 writer_template_i64)
configure_file(writer.in.f90 writer.f90 @ONLY)

file(READ writer_lt_template.in.f90 writer_lt_template)
configure_file(writer_lt.in.f90 writer_lt.f90 @ONLY)

set_directory_properties(PROPERTIES CMAKE_CONFIGURE_DEPENDS
"writer_template_r32.in.f90;writer_template_r64.in.f90;writer_template_i32.in.f90;writer_template_i64.in.f90;writer_lt_template.in.f90")

target_sources(h5fortran PRIVATE
  write.f90
  ${CMAKE_CURRENT_BINARY_DIR}/writer.f90
  ${CMAKE_CURRENT_BINARY_DIR}/writer_lt.f90)
