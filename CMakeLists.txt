cmake_minimum_required(VERSION 3.13...3.20)

project(h5fortran
  LANGUAGES C Fortran
  VERSION 3.5.1
  DESCRIPTION "thin, light object-oriented HDF5 Fortran interface"
  HOMEPAGE_URL https://github.com/geospace-code/h5fortran)

include(CTest)
if(NOT DEFINED ${PROJECT_NAME}_BUILD_TESTING)
  set(${PROJECT_NAME}_BUILD_TESTING ${BUILD_TESTING})
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

include(cmake/options.cmake)
include(cmake/compilers.cmake)
include(cmake/libraries.cmake)

# --- find or build HDF5 library

if(NOT hdf5_external)
  find_package(HDF5 COMPONENTS Fortran)
endif()

if(NOT HDF5_FOUND OR hdf5_external)
  include(cmake/build_hdf5.cmake)
endif()

if(HDF5_VERSION VERSION_LESS 1.8.7)
  message(WARNING "H5Fortran requires HDF5 >= 1.8.7")
endif()

# --- h5fortran library

add_library(h5fortran)
target_include_directories(h5fortran
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include  # for IBM XL
  INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
target_link_libraries(h5fortran PUBLIC HDF5::HDF5)
set_target_properties(h5fortran PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)

# GLOBAL needed for use by FetchContent
add_library(h5fortran::h5fortran INTERFACE IMPORTED GLOBAL)
target_link_libraries(h5fortran::h5fortran INTERFACE h5fortran)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod DESTINATION include)

install(TARGETS h5fortran
  EXPORT ${PROJECT_NAME}Targets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)


add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(src/tests)
endif()

include(cmake/install.cmake)

include(cmake/summary.cmake)
