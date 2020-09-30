cmake_minimum_required(VERSION 3.15)

if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(FATAL_ERROR "use cmake -B build or similar to avoid building in-source")
endif()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Debug or Release")
endif()

project(h5fortran
  LANGUAGES C Fortran
  VERSION 3.3.0
  DESCRIPTION "thin, light object-oriented HDF5 Fortran interface"
  HOMEPAGE_URL https://github.com/geospace-code/h5fortran)

include(CTest)

if(NOT DEFINED ${PROJECT_NAME}_BUILD_TESTING)
  set(${PROJECT_NAME}_BUILD_TESTING ${BUILD_TESTING})
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
# set(HDF5_FIND_DEBUG true)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/compilers.cmake)

add_library(h5fortran)  # declare here so dependencies may be setup

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/hdf5.cmake)

if(NOT HDF5OK)
  message(STATUS "h5fortran: HDF5 autobuild")
  add_dependencies(h5fortran HDF5proj)
endif()

# --- build
target_include_directories(h5fortran
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include  # for IBM XL
  INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>)
target_link_libraries(h5fortran PUBLIC HDF5::HDF5)
set_target_properties(h5fortran PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
add_library(h5fortran::h5fortran ALIAS h5fortran)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(src/tests)
endif()

if(PROJECT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/install.cmake)

  include(FeatureSummary)
  set_package_properties(Threads PROPERTIES DESCRIPTION "the system threads library")
  set_package_properties(HDF5 PROPERTIES URL "https://hdfgroup.org/" DESCRIPTION "fast, versatile file I/O format" TYPE REQUIRED)
  set_package_properties(ZLIB PROPERTIES URL "https://www.zlib.net/" DESCRIPTION "patent-free compression library")
  set_package_properties(SZIP PROPERTIES URL "http://www.compressconsult.com/szip/" DESCRIPTION "compression library")

  feature_summary(WHAT ALL)
endif()
