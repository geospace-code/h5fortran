# builds HDF5 library from scratch
# note: the use of "lib" vs. CMAKE_STATIC_LIBRARY_PREFIX is deliberate based on the particulars of these libraries
# across Intel Fortran on Windows vs. Gfortran on Windows vs. Linux.

set(hdf5_external true CACHE BOOL "autobuild HDF5")

set(HDF5_VERSION 1.10.7)
# for user information, not used by ExternalProject itself

include(ExternalProject)

if(NOT DEFINED HDF5_ROOT)
  set(HDF5_ROOT ${CMAKE_INSTALL_PREFIX})
endif()

set(HDF5_LIBRARIES)
foreach(_name hdf5_hl_fortran hdf5_hl_f90cstub hdf5_fortran hdf5_f90cstub hdf5_hl hdf5)
  list(APPEND HDF5_LIBRARIES ${HDF5_ROOT}/lib/lib${_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
endforeach()

set(HDF5_INCLUDE_DIRS ${HDF5_ROOT}/include)

# --- Zlib
set(zlib_root -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON -DZLIB_USE_EXTERNAL:BOOL=OFF)

include(${CMAKE_CURRENT_LIST_DIR}/build_zlib.cmake)

# --- HDF5
# https://forum.hdfgroup.org/t/issues-when-using-hdf5-as-a-git-submodule-and-using-cmake-with-add-subdirectory/7189/2

ExternalProject_Add(HDF5
URL ${hdf5_url}
URL_HASH SHA256=${hdf5_sha256}
UPDATE_DISCONNECTED ${EP_UPDATE_DISCONNECTED}
CONFIGURE_HANDLED_BY_BUILD ON
INACTIVITY_TIMEOUT 30
CMAKE_ARGS ${zlib_root} -DCMAKE_INSTALL_PREFIX:PATH=${HDF5_ROOT} -DHDF5_GENERATE_HEADERS:BOOL=false -DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=true -DBUILD_SHARED_LIBS:BOOL=false -DCMAKE_BUILD_TYPE=Release -DHDF5_BUILD_FORTRAN:BOOL=true -DHDF5_BUILD_CPP_LIB:BOOL=false -DHDF5_BUILD_TOOLS:BOOL=false -DBUILD_TESTING:BOOL=false -DHDF5_BUILD_EXAMPLES:BOOL=false
BUILD_BYPRODUCTS ${HDF5_LIBRARIES}
DEPENDS ZLIB)

# --- imported target

file(MAKE_DIRECTORY ${HDF5_INCLUDE_DIRS})
# avoid race condition

# this GLOBAL is required to be visible via other project's FetchContent of h5fortran
add_library(HDF5::HDF5 INTERFACE IMPORTED GLOBAL)
target_include_directories(HDF5::HDF5 INTERFACE "${HDF5_INCLUDE_DIRS}")
target_link_libraries(HDF5::HDF5 INTERFACE "${HDF5_LIBRARIES}")

add_dependencies(HDF5::HDF5 HDF5)

# --- external deps

target_link_libraries(HDF5::HDF5 INTERFACE ZLIB::ZLIB)

set(THREADS_PREFER_PTHREAD_FLAG true)
find_package(Threads)
if(Threads_FOUND)
  target_link_libraries(HDF5::HDF5 INTERFACE Threads::Threads)
endif(Threads_FOUND)

target_link_libraries(HDF5::HDF5 INTERFACE ${CMAKE_DL_LIBS})

if(UNIX)
  target_link_libraries(HDF5::HDF5 INTERFACE m)
endif(UNIX)
