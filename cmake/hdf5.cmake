# don't enclose this all in "if(NOT DEFINED HDF5OK)" because CMake intermittantly doesn't cache needed HDF5 variables.

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
  return()
endif()

if(NOT DEFINED HDF5OK)
  message(STATUS "HDF5 include: ${HDF5_INCLUDE_DIRS}")
  message(STATUS "HDF5 library: ${HDF5_LIBRARIES}")

  if(MSVC)
    include(${CMAKE_CURRENT_LIST_DIR}/win32_hdf5.cmake)
    win32_hdf5_env()
  endif(MSVC)
endif()

if(HDF5_FOUND)
  # MSVC check_fortran_source_runs needs to be set to PROJECT_BINARY_DIR.
  # may require vendoring, so we just do this workaround for now for MSVC.
  set(HDF5OK true CACHE BOOL "HDF5 library OK")
else()
  include(${CMAKE_CURRENT_LIST_DIR}/build_hdf5.cmake)
endif()
