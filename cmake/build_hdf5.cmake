# builds HDF5 library from scratch
# the if(NOT IS_DIRECTORY ..) statement is because otherwise CMake configures HDF5 each and every build cycle, which takes a minute or so.

if(NOT IS_DIRECTORY ${hdf5_bindir}/lib)

  set(hdf5_bindir ${PROJECT_BINARY_DIR}/hdf5 CACHE PATH "HDF5 library install location")

  include(ExternalProject)
  ExternalProject_Add(HDF5_proj
  GIT_REPOSITORY https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git
  GIT_TAG 1.12/master
  GIT_SHALLOW true
  UPDATE_DISCONNECTED true
  CONFIGURE_COMMAND ${CMAKE_CURRENT_BINARY_DIR}/HDF5_proj-prefix/src/HDF5_proj/configure --prefix=${hdf5_bindir} --enable-fortran --enable-build-mode=production --disable-tests --disable-tools --disable-shared
  BUILD_COMMAND make -j
  INSTALL_COMMAND make -j install
  )

  message(STATUS "installing HDF5 library in ${hdf5_bindir}")

  file(MAKE_DIRECTORY ${hdf5_bindir}/include)  # avoid race condition

  add_dependencies(h5fortran HDF5_proj)  # ensure HDF5 builds first
endif()

set(HDF5_LIBRARIES)
foreach(_l hdf5_hl_fortran hdf5_fortran hdf5_hl hdf5)
  list(APPEND HDF5_LIBRARIES ${hdf5_bindir}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}${_l}${CMAKE_STATIC_LIBRARY_SUFFIX})
endforeach()

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
INTERFACE_INCLUDE_DIRECTORIES ${hdf5_bindir}/include)

add_library(HDF5::HDF5 ALIAS hdf5)
