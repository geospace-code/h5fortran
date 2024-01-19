cmake_minimum_required(VERSION 3.19...3.28)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "Please do out of source build like
  cmake -Bbuild")
endif()

get_property(is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT is_multi_config AND NOT (CMAKE_BUILD_TYPE OR DEFINED ENV{CMAKE_BUILD_TYPE}))
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Release default")
endif()

project(h5fortran
LANGUAGES C Fortran
VERSION 4.10.5
)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# --- avoid Anaconda libraries
if(DEFINED ENV{CONDA_PREFIX})
  list(APPEND CMAKE_IGNORE_PREFIX_PATH $ENV{CONDA_PREFIX})
  list(APPEND CMAKE_IGNORE_PATH $ENV{CONDA_PREFIX}/bin)
  # need CMAKE_IGNORE_PATH for CMake < 3.23
  # and to ensure system env var PATH doesn't interfere
  # despite CMAKE_IGNORE_PREFIX_PATH
endif()

include(options.cmake)
include(cmake/compilers.cmake)
include(cmake/CheckHDF5.cmake)

if(find)
  find_package(HDF5 COMPONENTS Fortran)
endif()

if(HDF5_FOUND)
  check_hdf5()
else()
  include(cmake/hdf5.cmake)
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

# GLOBAL needed for use by parent projects
add_library(h5fortran::h5fortran INTERFACE IMPORTED GLOBAL)
target_link_libraries(h5fortran::h5fortran INTERFACE h5fortran)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
# avoid race condition when used by parent projects

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

install(TARGETS h5fortran EXPORT ${PROJECT_NAME}-targets)

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

file(GENERATE OUTPUT .gitignore CONTENT "*")
