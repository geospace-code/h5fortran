# builds HDF5 library from scratch
# note: the use of "lib" vs. CMAKE_STATIC_LIBRARY_PREFIX is deliberate based on the particulars of these libraries
# across Intel Fortran on Windows vs. Gfortran on Windows vs. Linux.

include(ExternalProject)

set(HDF5_LIBRARIES)
foreach(_name hdf5_hl_fortran hdf5_hl_f90cstub hdf5_fortran hdf5_f90cstub hdf5_hl hdf5)
  list(APPEND HDF5_LIBRARIES ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/bin/lib${_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
endforeach()

# NOTE: if the HDF5 CMake is allowed to rebuild, it will fail and this directory disappears (HDF5 1.12.0)
set(HDF5_INCLUDE_DIRS ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/bin/static)

if(EXISTS ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/bin/libhdf5_hl_fortran${CMAKE_STATIC_LIBRARY_SUFFIX})
  set(HDF5OK true CACHE BOOL "HDF5 OK")
endif()

file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/bin/static)  # avoid race condition

# --- Zlib
# always build Zlib to ensure compatibility. This is a common practice for HDF5.

if(WIN32)
  set(_zlib_name ${CMAKE_STATIC_LIBRARY_PREFIX}zlibstatic${CMAKE_STATIC_LIBRARY_SUFFIX})
else()
  set(_zlib_name ${CMAKE_STATIC_LIBRARY_PREFIX}z${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(_zlib_file ${PROJECT_BINARY_DIR}/ZLIBproj-prefix/src/ZLIBproj-build/${_zlib_name})
set(_zlib_build ${PROJECT_BINARY_DIR}/ZLIBproj-prefix/src/ZLIBproj-build)
set(_zlib_h ${PROJECT_BINARY_DIR}/ZLIBproj-prefix/src/ZLIBproj/zlib.h)

ExternalProject_Add(ZLIBproj
URL https://zlib.net/zlib1211.zip
URL_HASH SHA1=bccd93ad3cee39c3d08eee68d45b3e11910299f2
UPDATE_DISCONNECTED true
CMAKE_ARGS -DCMAKE_BUILD_TYPE=Release
BUILD_BYPRODUCTS ${_zlib_file}
INSTALL_COMMAND ""
)

if(NOT IS_DIRECTORY ${_zlib_build})
  file(MAKE_DIRECTORY ${_zlib_build})  # avoid race condition
endif()
if(NOT EXISTS ${_zlib_build}/zlib.h)
  file(CREATE_LINK ${_zlib_h} ${_zlib_build}/zlib.h SYMBOLIC)
endif()

add_library(ZLIB::ZLIB INTERFACE IMPORTED GLOBAL)
target_link_libraries(ZLIB::ZLIB INTERFACE ${_zlib_file})
target_include_directories(ZLIB::ZLIB INTERFACE ${_zlib_build})

set(_zlib_root -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON -DZLIB_ROOT:PATH=${_zlib_build} -DZLIB_LIBRARY:FILEPATH=${_zlib_file} -DZLIB_INCLUDE_DIR:PATH=${_zlib_build} -DZLIB_USE_EXTERNAL:BOOL=OFF)

# --- HDF5
# https://forum.hdfgroup.org/t/issues-when-using-hdf5-as-a-git-submodule-and-using-cmake-with-add-subdirectory/7189/2

if(HDF5OK)
  # this if() statement is to avoid bugs in HDF5 from constantly rebuilding HDF5 on Linux (1.10.7 and 1.12.0 at least)
  add_custom_target(HDF5proj DEPENDS ${HDF5_LIBRARIES})
  add_custom_command(OUTPUT ${HDF5_LIBRARIES})
else()
  ExternalProject_Add(HDF5proj
  GIT_REPOSITORY https://github.com/HDFGroup/hdf5.git
  GIT_TAG hdf5_1_10_7
  GIT_SHALLOW true
  # URL https://hdf-wordpress-1.s3.amazonaws.com/wp-content/uploads/manual/HDF5/HDF5_1_12_0/source/CMake-hdf5-1.12.0.tar.gz
  # URL_HASH MD5=33ab3d5b9019ca468364d226e0ccdea6
  UPDATE_DISCONNECTED true
  CMAKE_ARGS ${_zlib_root} -DHDF5_GENERATE_HEADERS:BOOL=false -DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=true -DBUILD_SHARED_LIBS:BOOL=false -DCMAKE_BUILD_TYPE=Release -DHDF5_BUILD_FORTRAN:BOOL=true -DHDF5_BUILD_CPP_LIB:BOOL=false -DHDF5_BUILD_TOOLS:BOOL=false -DBUILD_TESTING:BOOL=false -DHDF5_BUILD_EXAMPLES:BOOL=false
  BUILD_BYPRODUCTS ${HDF5_LIBRARIES}
  INSTALL_COMMAND ""
  )

  add_dependencies(HDF5proj ZLIBproj)
endif(HDF5OK)
# --- external deps

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
target_link_libraries(HDF5::HDF5 INTERFACE ${HDF5_LIBRARIES})
target_include_directories(HDF5::HDF5 INTERFACE ${HDF5_INCLUDE_DIRS})
