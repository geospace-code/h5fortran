cmake_minimum_required(VERSION 3.11...3.25)

get_property(is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT is_multi_config AND NOT (CMAKE_BUILD_TYPE OR DEFINED ENV{CMAKE_BUILD_TYPE}))
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Release default")
endif()

project(h5fortran
LANGUAGES C Fortran
VERSION 4.10.2
)

include(CTest)
if(NOT DEFINED ${PROJECT_NAME}_BUILD_TESTING)
  set(${PROJECT_NAME}_BUILD_TESTING ${BUILD_TESTING})
endif()

include(cmake/options.cmake)
include(cmake/compilers.cmake)

find_package(HDF5 COMPONENTS Fortran)
if(NOT HDF5_FOUND)
  message(FATAL_ERROR "HDF5 not found or working on the system. First build HDF5 by:

  cmake -S ${CMAKE_CURRENT_SOURCE_DIR}/scripts -B build -DCMAKE_INSTALL_PREFIX=~/mylibs
  cmake --build build

  The '~/mylibs' is an arbitrary directory.
  Then build h5fortran, referring to this HDF5 installation:

  cmake -B build -DCMAKE_PREFIX_PATH=~/mylibs
  cmake --build build
  ")
endif()

if(HDF5_VERSION VERSION_LESS 1.8.7)
  message(WARNING "HDF5 >= 1.8.7 required for ${PROJECT_NAME}")
endif()

if(hdf5_parallel OR HDF5_HAVE_PARALLEL)
  target_link_libraries(HDF5::HDF5 INTERFACE MPI::MPI_Fortran)
endif()

# --- h5fortran library

add_library(h5fortran)
target_include_directories(h5fortran PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>
)
target_link_libraries(h5fortran PUBLIC HDF5::HDF5)
set_property(TARGET h5fortran PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET h5fortran PROPERTY VERSION ${PROJECT_VERSION})

# GLOBAL needed for use by FetchContent
add_library(h5fortran::h5fortran INTERFACE IMPORTED GLOBAL)
target_link_libraries(h5fortran::h5fortran INTERFACE h5fortran)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

install(TARGETS h5fortran EXPORT ${PROJECT_NAME}-targets
LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(test)
endif()

# additional Find*.cmake necessary
install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindHDF5.cmake
DESTINATION cmake
)

include(cmake/pkgconf.cmake)
include(cmake/install.cmake)
