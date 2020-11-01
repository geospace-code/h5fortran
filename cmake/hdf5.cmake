if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.19)
  # make missing imported targets fail immediately
  cmake_policy(SET CMP0111 NEW)
endif()

if(hdf5_external)
  include(${CMAKE_CURRENT_LIST_DIR}/build_hdf5.cmake)
  return()
endif()

find_package(HDF5 COMPONENTS Fortran HL)

if(NOT HDF5_FOUND)
  include(${CMAKE_CURRENT_LIST_DIR}/build_hdf5.cmake)
endif()
