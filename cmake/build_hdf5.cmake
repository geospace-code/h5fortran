# builds HDF5 library from scratch

set(HDF5_LIBRARIES)
foreach(_name hdf5_hl_fortran hdf5_hl_f90cstub hdf5_fortran hdf5_f90cstub hdf5_hl hdf5)
  list(APPEND HDF5_LIBRARIES ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/bin/${CMAKE_STATIC_LIBRARY_PREFIX}${_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
endforeach()

set(HDF5_INCLUDE_DIRS
 ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/bin/static)

if(EXISTS ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/lib/${CMAKE_STATIC_LIBRARY_PREFIX}hdf5_hl_fortran${CMAKE_STATIC_LIBRARY_SUFFIX})
  set(HDF5_FOUND true CACHE BOOL "self-built HDF5")
  set(HDF5OK true CACHE BOOL "HDF5 OK")
endif()

include(ExternalProject)
ExternalProject_Add(HDF5proj
GIT_REPOSITORY https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git
GIT_TAG 1.12/master
GIT_SHALLOW true
# URL https://hdf-wordpress-1.s3.amazonaws.com/wp-content/uploads/manual/HDF5/HDF5_1_12_0/source/CMake-hdf5-1.12.0.tar.gz
# URL_HASH MD5=33ab3d5b9019ca468364d226e0ccdea6
UPDATE_DISCONNECTED true
CMAKE_ARGS -DBUILD_SHARED_LIBS:BOOL=false -DCMAKE_BUILD_TYPE=Release -DHDF5_BUILD_FORTRAN:BOOL=true -DHDF5_BUILD_CPP_LIB:BOOL=false -DHDF5_BUILD_TOOLS:BOOL=false -DBUILD_TESTING:BOOL=false -DHDF5_BUILD_EXAMPLES:BOOL=false
BUILD_BYPRODUCTS ${HDF5_LIBRARIES}
INSTALL_COMMAND ""
)

file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/bin/static)  # avoid race condition

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
