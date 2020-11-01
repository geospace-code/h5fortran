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
  set(HDF5_FOUND true)
endif()

file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/HDF5proj-prefix/src/HDF5proj-build/bin/static)  # avoid race condition

# --- Zlib
set(zlib_root -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON -DZLIB_USE_EXTERNAL:BOOL=OFF)
if(TARGET ZLIB::ZLIB)
  add_custom_target(ZLIBproj)
else()
  include(${CMAKE_CURRENT_LIST_DIR}/zlib_setup.cmake)
endif()

# --- HDF5
# https://forum.hdfgroup.org/t/issues-when-using-hdf5-as-a-git-submodule-and-using-cmake-with-add-subdirectory/7189/2


if(NOT HDF5_FOUND)
  ExternalProject_Add(HDF5proj
  GIT_REPOSITORY https://github.com/HDFGroup/hdf5.git
  GIT_TAG 1.10/master
  GIT_SHALLOW true
  # URL https://hdf-wordpress-1.s3.amazonaws.com/wp-content/uploads/manual/HDF5/HDF5_1_12_0/source/CMake-hdf5-1.12.0.tar.gz
  # URL_HASH MD5=33ab3d5b9019ca468364d226e0ccdea6
  UPDATE_DISCONNECTED true
  CMAKE_ARGS ${zlib_root} -DHDF5_GENERATE_HEADERS:BOOL=false -DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=true -DBUILD_SHARED_LIBS:BOOL=false -DCMAKE_BUILD_TYPE=Release -DHDF5_BUILD_FORTRAN:BOOL=true -DHDF5_BUILD_CPP_LIB:BOOL=false -DHDF5_BUILD_TOOLS:BOOL=false -DBUILD_TESTING:BOOL=false -DHDF5_BUILD_EXAMPLES:BOOL=false
  BUILD_BYPRODUCTS ${HDF5_LIBRARIES}
  INSTALL_COMMAND ""
  )

  add_dependencies(HDF5proj ZLIBproj)
endif()

# this GLOBAL is required to be visible via FetchContent
add_library(HDF5::HDF5 INTERFACE IMPORTED GLOBAL)
target_include_directories(HDF5::HDF5 INTERFACE "${HDF5_INCLUDE_DIRS}")
target_link_libraries(HDF5::HDF5 INTERFACE "${HDF5_LIBRARIES}")

if(NOT HDF5_FOUND)
  add_dependencies(HDF5::HDF5 HDF5proj)
endif()

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

set(hdf5_external true CACHE BOOL "autobuild HDF5")
