# builds HDF5 library from scratch
# Keeps rebuilding -- not working

if(NOT _hdf5_bindir)
  set(_hdf5_bindir ${PROJECT_BINARY_DIR}/hdf5-${PROJECT_VERSION})
endif()

set(HDF5_LIBRARIES)
foreach(_name hdf5_hl_fortran hdf5_hl_f90cstub hdf5_fortran hdf5_f90cstub hdf5_hl hdf5)
  list(APPEND HDF5_LIBRARIES ${_hdf5_bindir}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}${_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
endforeach()

set(HDF5_INCLUDE_DIRS ${_hdf5_bindir}/include ${_hdf5_bindir}/include/static)

if(EXISTS ${_hdf5_bindir}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}hdf5_hl_fortran${CMAKE_STATIC_LIBRARY_SUFFIX})
  set(HDF5_FOUND true CACHE BOOL "self-built HDF5")
endif()

include(ExternalProject)
ExternalProject_Add(HDF5proj
GIT_REPOSITORY https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git
GIT_TAG 1.12/master
GIT_SHALLOW true
UPDATE_DISCONNECTED true
CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${_hdf5_bindir} -DBUILD_SHARED_LIBS:BOOL=false -DCMAKE_BUILD_TYPE=Release -DHDF5_BUILD_FORTRAN:BOOL=true -DHDF5_BUILD_CPP_LIB:BOOL=false -DHDF5_BUILD_TOOLS:BOOL=false -DBUILD_TESTING:BOOL=false -DHDF5_BUILD_EXAMPLES:BOOL=false
INSTALL_COMMAND cmake --install ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build
BUILD_BYPRODUCTS ${HDF5_LIBRARIES}
)

file(MAKE_DIRECTORY ${_hdf5_bindir}/include/static)  # avoid race condition

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

# this GLOBAL is required to be visible via FetchContent
add_library(HDF5::HDF5 INTERFACE IMPORTED GLOBAL)
set_target_properties(HDF5::HDF5 PROPERTIES
INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}")
