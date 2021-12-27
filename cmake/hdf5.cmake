# builds HDF5 library from scratch
# note: the use of "lib" vs. CMAKE_*_LIBRARY_PREFIX is deliberate based on HDF5
# across Intel Fortran on Windows (MSVC-like) vs. Gfortran on Windows vs. Linux.

include(ExternalProject)

set(hdf5_external true CACHE BOOL "autobuild HDF5")

if(hdf5_parallel)
  find_package(MPI REQUIRED COMPONENTS C)
endif()

# need to be sure _ROOT isn't empty, DEFINED is not enough
if(NOT HDF5_ROOT)
  set(HDF5_ROOT ${CMAKE_INSTALL_PREFIX})
endif()

set(HDF5_LIBRARIES)
set(HDF5_DLLS)
foreach(_name hdf5_hl_fortran hdf5_hl_f90cstub hdf5_fortran hdf5_f90cstub hdf5_hl hdf5)
  if(BUILD_SHARED_LIBS)
    list(APPEND HDF5_LIBRARIES ${HDF5_ROOT}/lib/lib${_name}$<IF:$<BOOL:${MSVC}>,${CMAKE_STATIC_LIBRARY_SUFFIX},${CMAKE_SHARED_LIBRARY_SUFFIX}>$<$<BOOL:${MINGW}>:.a>)
    list(APPEND HDF5_DLLS $<$<BOOL:${WIN32}>:${HDF5_ROOT}/bin/lib${_name}.dll>)
  else()
    list(APPEND HDF5_LIBRARIES ${HDF5_ROOT}/lib/lib${_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
  endif()
endforeach()

set(HDF5_INCLUDE_DIRS ${HDF5_ROOT}/include)

# --- Zlib
set(zlib_root
-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON
-DZLIB_USE_EXTERNAL:BOOL=OFF)

if(NOT TARGET ZLIB::ZLIB)
  include(${CMAKE_CURRENT_LIST_DIR}/zlib.cmake)
endif()
# --- HDF5
# https://forum.hdfgroup.org/t/issues-when-using-hdf5-as-a-git-submodule-and-using-cmake-with-add-subdirectory/7189/2

set(hdf5_cmake_args
${zlib_root}
-DCMAKE_INSTALL_PREFIX:PATH=${HDF5_ROOT}
-DCMAKE_MODULE_PATH:PATH=${CMAKE_MODULE_PATH}
-DHDF5_GENERATE_HEADERS:BOOL=false
-DHDF5_DISABLE_COMPILER_WARNINGS:BOOL=true
-DBUILD_STATIC_LIBS:BOOL=$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
-DCMAKE_BUILD_TYPE=Release
-DHDF5_BUILD_FORTRAN:BOOL=true
-DHDF5_BUILD_CPP_LIB:BOOL=false
-DBUILD_TESTING:BOOL=false
-DHDF5_BUILD_EXAMPLES:BOOL=false
-DUSE_LIBAEC:bool=true
-DHDF5_BUILD_TOOLS:BOOL=$<NOT:$<BOOL:${hdf5_parallel}>>
-DHDF5_ENABLE_PARALLEL:BOOL=$<BOOL:${hdf5_parallel}>
-DMPI_ROOT:PATH=${MPI_ROOT}
)
# https://github.com/HDFGroup/hdf5/issues/818  for broken ph5diff in HDF5_BUILD_TOOLS

ExternalProject_Add(HDF5
URL ${hdf5_url}
URL_HASH SHA256=${hdf5_sha256}
CMAKE_ARGS ${hdf5_cmake_args}
CMAKE_GENERATOR ${EXTPROJ_GENERATOR}
BUILD_BYPRODUCTS ${HDF5_LIBRARIES}
DEPENDS ZLIB::ZLIB
CONFIGURE_HANDLED_BY_BUILD ON
INACTIVITY_TIMEOUT 15
)

# --- imported target

file(MAKE_DIRECTORY ${HDF5_INCLUDE_DIRS})
# avoid race condition

# this GLOBAL is required to be visible via other project's FetchContent of h5fortran
add_library(HDF5::HDF5 INTERFACE IMPORTED GLOBAL)
target_include_directories(HDF5::HDF5 INTERFACE "${HDF5_INCLUDE_DIRS}")
target_link_libraries(HDF5::HDF5 INTERFACE "${HDF5_LIBRARIES}")

add_dependencies(HDF5::HDF5 HDF5)

# --- HDF5 parallel compression support
# this could be improved by making it an ExternalProject post-build step instead of assumptions made here
if(hdf5_parallel)
  if(MPI_VERSION VERSION_GREATER_EQUAL 3)
    message(STATUS "Building HDF5-MPI: MPI-3 available, assuming HDF5 parallel compression enabled")
    set(hdf5_parallel_compression ".true." CACHE STRING "configure variable for HDF5 parallel compression")
  else()
    message(STATUS "Building HDF5-MPI: MPI-3 NOT available => HDF5 parallel compression disabled")
    set(hdf5_parallel_compression ".false." CACHE STRING "configure variable for HDF5 parallel compression")
  endif()
endif(hdf5_parallel)

# --- external deps
find_package(Threads)

target_link_libraries(HDF5::HDF5 INTERFACE
ZLIB::ZLIB
${CMAKE_THREAD_LIBS_INIT}
${CMAKE_DL_LIBS}
$<$<BOOL:${UNIX}>:m>
)
# libdl and libm are needed on some systems

# --- dynamic shared HDF5

set(CMAKE_INSTALL_NAME_DIR ${CMAKE_INSTALL_PREFIX}/lib)
set(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib)
