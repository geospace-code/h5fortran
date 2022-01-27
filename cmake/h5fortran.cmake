include(ExternalProject)

if(NOT hdf5_external)
  # h5fortran inside if() because h5fortran config calls find_package(HDF5)
  # disabled h5fortran search for now because we're undergoing rapid devel soon for MPI.
  find_package(h5fortran CONFIG QUIET)
  if(h5fortran_FOUND)
    message(STATUS "Found h5fortran ${h5fortran_DIR}")
    return()
  endif()

  if(autobuild)
    find_package(HDF5 COMPONENTS Fortran)
  else()
    find_package(HDF5 COMPONENTS Fortran REQUIRED)
  endif()
endif()

if(NOT HDF5_FOUND OR hdf5_external)
  include(${CMAKE_CURRENT_LIST_DIR}/hdf5.cmake)
endif()

if(NOT h5fortran_ROOT)
  set(h5fortran_ROOT ${CMAKE_INSTALL_PREFIX})
endif()

find_package(ZLIB)

set(h5fortran_INCLUDE_DIRS ${h5fortran_ROOT}/include)

set(h5fortran_implib)
if(BUILD_SHARED_LIBS)
  if(WIN32)
    set(h5fortran_implib ${h5fortran_ROOT}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}h5fortran${CMAKE_SHARED_LIBRARY_SUFFIX}${CMAKE_STATIC_LIBRARY_SUFFIX})
    set(h5fortran_LIBRARIES ${h5fortran_ROOT}/bin/${CMAKE_SHARED_LIBRARY_PREFIX}h5fortran${CMAKE_SHARED_LIBRARY_SUFFIX})
  else()
    set(h5fortran_LIBRARIES ${h5fortran_ROOT}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}h5fortran${CMAKE_SHARED_LIBRARY_SUFFIX})
  endif()
else()
  set(h5fortran_LIBRARIES ${h5fortran_ROOT}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}h5fortran${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(h5fortran_cmake_args
-DCMAKE_INSTALL_PREFIX=${h5fortran_ROOT}
-DZLIB_ROOT:PATH=${ZLIB_ROOT}
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
-DCMAKE_BUILD_TYPE=Release
-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
-DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
-DBUILD_TESTING:BOOL=false
-Dautobuild:BOOL=false
-DHDF5_ROOT:PATH=${HDF5_ROOT}
)

ExternalProject_Add(H5FORTRAN
SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/..
CMAKE_ARGS ${h5fortran_cmake_args}
CMAKE_GENERATOR ${EXTPROJ_GENERATOR}
BUILD_BYPRODUCTS ${h5fortran_LIBRARIES}
INACTIVITY_TIMEOUT 15
CONFIGURE_HANDLED_BY_BUILD ON
DEPENDS HDF5::HDF5
)

file(MAKE_DIRECTORY ${h5fortran_INCLUDE_DIRS})

if(BUILD_SHARED_LIBS)
  add_library(h5fortran::h5fortran SHARED IMPORTED)
  if(WIN32)
    set_target_properties(h5fortran::h5fortran PROPERTIES IMPORTED_IMPLIB ${h5fortran_implib})
  endif()
else()
  add_library(h5fortran::h5fortran STATIC IMPORTED)
endif()

set_target_properties(h5fortran::h5fortran PROPERTIES IMPORTED_LOCATION ${h5fortran_LIBRARIES})
target_include_directories(h5fortran::h5fortran INTERFACE ${h5fortran_INCLUDE_DIRS})
target_link_libraries(h5fortran::h5fortran INTERFACE HDF5::HDF5)

# race condition for linking without this
add_dependencies(h5fortran::h5fortran H5FORTRAN)
