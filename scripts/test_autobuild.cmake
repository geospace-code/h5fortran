# test that HDF5 autobuild works across HDF5 versions
cmake_minimum_required(VERSION 3.21)

set(hdf5_versions 1.10 1.14 dev)

foreach(v IN LISTS hdf5_versions)
  message(STATUS "Testing auto-build of HDF5 version ${v}")

  execute_process(COMMAND ${CMAKE_COMMAND} -B /tmp/build_hdf5_${v} -S ${CMAKE_CURRENT_LIST_DIR}/..
    -Dh5fortran_hdf5_req=${v} -Dh5fortran_find=no
    COMMAND_ERROR_IS_FATAL ANY
  )

  execute_process(COMMAND ${CMAKE_COMMAND} --build /tmp/build_hdf5_${v}
    COMMAND_ERROR_IS_FATAL ANY
  )

  execute_process(COMMAND ${CMAKE_CTEST_COMMAND} --test-dir /tmp/build_hdf5_${v} --output-on-failure
    COMMAND_ERROR_IS_FATAL ANY
  )
endforeach()
