if(concepts)
add_subdirectory(concepts)
endif(concepts)

add_subdirectory(read)

add_subdirectory(write)

target_sources(h5fortran PRIVATE
interface.f90 attributes.f90
)
