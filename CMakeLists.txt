cmake_minimum_required(VERSION 3.15)

include(cmake/policy.cmake)
include(cmake/compiler_find.cmake)

project(h5fortran
  LANGUAGES C Fortran
  VERSION 3.4.2
  DESCRIPTION "thin, light object-oriented HDF5 Fortran interface"
  HOMEPAGE_URL https://github.com/geospace-code/h5fortran)

include(CTest)
if(NOT DEFINED ${PROJECT_NAME}_BUILD_TESTING)
  set(${PROJECT_NAME}_BUILD_TESTING ${BUILD_TESTING})
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/compilers.cmake)

# --- find or build HDF5 library

find_package(HDF5 COMPONENTS Fortran)

if(NOT HDF5_FOUND)
  include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/build_hdf5.cmake)
endif()

if(HDF5_VERSION VERSION_LESS 1.8.7)
  message(STATUS "H5Fortran requires HDF5 >= 1.8.7")
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
add_library(h5fortran::h5fortran ALIAS h5fortran)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(src/tests)
endif()

if(PROJECT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/install.cmake)
endif()

include(FeatureSummary)
set_package_properties(Threads PROPERTIES DESCRIPTION "the system threads library")
set_package_properties(HDF5 PROPERTIES URL "https://hdfgroup.org/" DESCRIPTION "fast, versatile file I/O format" TYPE REQUIRED)
set_package_properties(ZLIB PROPERTIES URL "https://www.zlib.net/" DESCRIPTION "patent-free compression library")
set_package_properties(SZIP PROPERTIES URL "http://www.compressconsult.com/szip/" DESCRIPTION "compression library")

add_feature_info(AutoHDF5 hdf5_external "auto-build HDF5")

feature_summary(WHAT ALL)
