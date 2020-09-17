# builds HDF5 library from scratch

function(find_self_hdf5 bindir)

set(HDF5_LIBRARIES)
foreach(_name hdf5_hl_fortran hdf5_hl_f90cstub hdf5_fortran hdf5_f90cstub hdf5_hl hdf5)
  find_library(_lib_${_name}
    NAMES ${_name}
    PATHS ${_hdf5_bindir}
    PATH_SUFFIXES lib
    NO_DEFAULT_PATH)

  if(NOT _lib_${_name})
    message(STATUS "did not find ${_lib_${_name}}")
    unset(_lib_${_name} CACHE)
    return()
  endif()

  list(APPEND HDF5_LIBRARIES ${_lib_${_name}})
endforeach()

message(STATUS "using HDF5 ${HDF5_LIBRARIES}")

find_path(HDF5_MODULE_DIR
  NAMES hdf5.mod
  HINTS ${HDF5_INCLUDE_DIRS}
  PATH_SUFFIXES static
  NO_DEFAULT_PATH)
if(HDF5_MODULE_DIR)
  message(STATUS "Found ${HDF5_MODULE_DIR}")
  list(APPEND HDF5_INCLUDE_DIRS ${HDF5_MODULE_DIR})
  set(HDF5_INCLUDE_DIRS ${HDF5_INCLUDE_DIRS} PARENT_SCOPE)
else()
  unset(HDF5_MODULE_DIR CACHE)
endif()

message(STATUS "include HDF5 ${HDF5_INCLUDE_DIRS}")

set(HDF5_LIBRARIES ${HDF5_LIBRARIES} PARENT_SCOPE)
set(HDF5_FOUND true CACHE BOOL "HDF5 library found" FORCE)

endfunction(find_self_hdf5)

# --- script
if(NOT _hdf5_bindir)
  set(_hdf5_bindir ${PROJECT_BINARY_DIR}/hdf5)
endif()

set(HDF5_INCLUDE_DIRS ${_hdf5_bindir}/include)
# HDF5_INCLUDE_DIRS before this!
find_self_hdf5(${_hdf5_bindir})

if(NOT HDF5_FOUND)

include(ExternalProject)
# keep include(ExternalProject) inside if(NOT HDF5_FOUND) or it will fail randomly

ExternalProject_Add(HDF5_proj
GIT_REPOSITORY https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git
GIT_TAG 1.12/master
GIT_SHALLOW true
UPDATE_DISCONNECTED true
CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${_hdf5_bindir} -DBUILD_SHARED_LIBS:BOOL=false -DCMAKE_BUILD_TYPE=Release -DHDF5_BUILD_FORTRAN:BOOL=true -DHDF5_BUILD_CPP_LIB:BOOL=false -DHDF5_BUILD_TOOLS:BOOL=false -DBUILD_TESTING:BOOL=false -DHDF5_BUILD_EXAMPLES:BOOL=false
INSTALL_COMMAND cmake --install ${PROJECT_BINARY_DIR}/HDF5_proj-prefix/src/HDF5_proj-build
)

file(MAKE_DIRECTORY ${HDF5_INCLUDE_DIRS})  # avoid race condition

add_dependencies(h5fortran HDF5_proj)  # ensure HDF5 builds first

find_self_hdf5(${_hdf5_bindir})

endif()

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
