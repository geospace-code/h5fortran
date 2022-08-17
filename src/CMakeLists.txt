set(s ${CMAKE_CURRENT_SOURCE_DIR})

target_sources(h5fortran PRIVATE
${s}/utils.f90
${s}/read.f90 ${s}/read_scalar.f90 ${s}/read_ascii.f90 ${s}/reader.f90
${s}/write.f90 ${s}/write_scalar.f90 ${s}/writer.f90
${s}/reader_lt.f90 ${s}/writer_lt.f90
${s}/interface.f90
${s}/attr.f90
${s}/attr_read.f90
${s}/attr_write.f90
)
