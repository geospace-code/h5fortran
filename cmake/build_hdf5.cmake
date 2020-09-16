# builds HDF5 library from scratch

set(HDF5_ROOT ${PROJECT_BINARY_DIR}/hdf5 CACHE PATH "HDF5 library install location")

if(IS_DIRECTORY ${HDF5_ROOT})
  # already installed, disable long configure step and verbose build, install steps
  set(_cmd0 "")
  set(_cmd1 "")
  set(_cmd2 "")
else()
  message(STATUS "installing HDF5 library in ${HDF5_ROOT}")
  set(_cmd0 ${CMAKE_CURRENT_BINARY_DIR}/HDF5_proj-prefix/src/HDF5_proj/configure --prefix=${HDF5_ROOT} --enable-fortran --enable-build-mode=production --disable-tests --disable-tools --disable-shared)
  set(_cmd1 make -j)
  set(_cmd2 make -j install)
endif()

set(HDF5_LIBRARIES)
foreach(_l hdf5_hl_fortran hdf5_fortran hdf5_hl hdf5)
  list(APPEND HDF5_LIBRARIES ${HDF5_ROOT}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}${_l}${CMAKE_STATIC_LIBRARY_SUFFIX})
endforeach()

include(ExternalProject)
ExternalProject_Add(HDF5_proj
GIT_REPOSITORY https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git
GIT_TAG 1.12/master
GIT_SHALLOW true
UPDATE_DISCONNECTED true
CONFIGURE_COMMAND "${_cmd0}"
BUILD_COMMAND "${_cmd1}"
BUILD_BYPRODUCTS "${HDF5_LIBRARIES}"
INSTALL_COMMAND "${_cmd2}"
)

file(MAKE_DIRECTORY ${HDF5_ROOT}/include)  # avoid race condition

add_dependencies(h5fortran HDF5_proj)  # ensure HDF5 builds first

find_package(ZLIB REQUIRED)
list(APPEND HDF5_LIBRARIES ZLIB::ZLIB)

set(THREADS_PREFER_PTHREAD_FLAG true)
find_package(Threads)
if(Threads_FOUND)
list(APPEND HDF5_LIBRARIES Threads::Threads)
endif(Threads_FOUND)

list(APPEND HDF5_LIBRARIES ${CMAKE_DL_LIBS})

if(UNIX)
list(APPEND HDF5_LIBRARIES m)
endif(UNIX)

message(VERBOSE " using self-built ${HDF5_LIBRARIES}")

add_library(hdf5 INTERFACE IMPORTED)
set_target_properties(hdf5 PROPERTIES
INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
INTERFACE_INCLUDE_DIRECTORIES ${HDF5_ROOT}/include)

add_library(HDF5::HDF5 ALIAS hdf5)
