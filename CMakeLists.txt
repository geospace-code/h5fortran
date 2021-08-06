cmake_minimum_required(VERSION 3.20...3.21)

file(STRINGS ${CMAKE_CURRENT_SOURCE_DIR}/VERSION PROJECT_VERSION
  REGEX "^([0-9]+\.[0-9]+\.[0-9]+)" LIMIT_INPUT 16 LENGTH_MAXIMUM 16 LIMIT_COUNT 1)

project(h5fortran
  LANGUAGES C Fortran
  VERSION ${PROJECT_VERSION}
  DESCRIPTION "thin, light object-oriented HDF5 Fortran interface"
  HOMEPAGE_URL https://github.com/geospace-code/h5fortran)

include(CTest)
if(NOT DEFINED ${PROJECT_NAME}_BUILD_TESTING)
  set(${PROJECT_NAME}_BUILD_TESTING ${BUILD_TESTING})
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

include(cmake/options.cmake)
include(cmake/compilers.cmake)

# --- find or build HDF5 library

if(NOT hdf5_external)
  if(autobuild)
    find_package(HDF5 COMPONENTS Fortran HL)
  else()
    find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
  endif()
endif()

if(NOT HDF5_FOUND OR hdf5_external)
  include(cmake/build_hdf5.cmake)
endif()

if(HDF5_VERSION VERSION_LESS 1.8.7)
  message(WARNING "H5Fortran requires HDF5 >= 1.8.7")
endif()

if(hdf5_parallel OR HDF5_IS_PARALLEL)
  find_package(MPI COMPONENTS Fortran REQUIRED)
  target_link_libraries(HDF5::HDF5 INTERFACE MPI::MPI_Fortran)
endif()

# --- h5fortran library

add_library(h5fortran)
target_include_directories(h5fortran
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include  # for IBM XL
  INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
target_link_libraries(h5fortran PUBLIC HDF5::HDF5)
set_target_properties(h5fortran PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
  LABELS core
  VERSION ${PROJECT_VERSION})

# GLOBAL needed for use by FetchContent
add_library(h5fortran::h5fortran INTERFACE IMPORTED GLOBAL)
target_link_libraries(h5fortran::h5fortran INTERFACE h5fortran)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod TYPE INCLUDE)

install(TARGETS h5fortran EXPORT ${PROJECT_NAME}Targets)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(src/tests)
endif()

# additional Find*.cmake necessary
install(FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/FindHDF5.cmake
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/FindSZIP.cmake
  DESTINATION lib/cmake/Modules)

include(cmake/pkgconf.cmake)
include(cmake/install.cmake)

include(cmake/summary.cmake)
