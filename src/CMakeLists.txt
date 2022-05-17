if(concepts)
  add_subdirectory(concepts)
endif(concepts)

add_subdirectory(read)

target_sources(h5fortran PRIVATE
write.f90 write_scalar.f90 writer.f90 writer_lt.f90
interface.f90 attributes.f90
)
