if(concepts)
add_subdirectory(concepts)
endif(concepts)

if(WIN32)
  set(is_windows .true.)
else()
  set(is_windows .false.)
endif()
configure_file(pathlib.in.f90 pathlib.f90 @ONLY)

add_subdirectory(read)

add_subdirectory(write)

target_sources(h5fortran PRIVATE
interface.f90 attributes.f90
${CMAKE_CURRENT_BINARY_DIR}/pathlib.f90
)
