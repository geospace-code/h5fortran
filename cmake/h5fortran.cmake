include(ExternalProject)

if(NOT hdf5_external)
  # h5fortran inside if() because h5fortran config calls find_package(HDF5)
  find_package(h5fortran CONFIG QUIET)
  if(h5fortran_FOUND)
    message(STATUS "Found h5fortran ${h5fortran_DIR}")
    return()
  endif()

  find_package(HDF5 COMPONENTS Fortran REQUIRED)
endif()

if(NOT HDF5_FOUND OR hdf5_external)
  include(${CMAKE_CURRENT_LIST_DIR}/hdf5.cmake)
endif()

set(h5fortran_INCLUDE_DIRS ${CMAKE_INSTALL_PREFIX}/include)

if(BUILD_SHARED_LIBS)
  if(WIN32)
    set(h5fortran_LIBRARIES ${CMAKE_INSTALL_PREFIX}/bin/${CMAKE_SHARED_LIBRARY_PREFIX}h5fortran${CMAKE_SHARED_LIBRARY_SUFFIX})
  else()
    set(h5fortran_LIBRARIES ${CMAKE_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}h5fortran${CMAKE_SHARED_LIBRARY_SUFFIX})
  endif()
else()
  set(h5fortran_LIBRARIES ${CMAKE_INSTALL_PREFIX}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}h5fortran${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(h5fortran_cmake_args
-DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
-DCMAKE_PREFIX_PATH:PATH=${CMAKE_INSTALL_PREFIX}
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
BUILD_BYPRODUCTS ${h5fortran_LIBRARIES}
INACTIVITY_TIMEOUT 15
CONFIGURE_HANDLED_BY_BUILD ON
DEPENDS HDF5::HDF5
)

file(MAKE_DIRECTORY ${h5fortran_INCLUDE_DIRS})

add_library(h5fortran::h5fortran INTERFACE IMPORTED GLOBAL)
target_include_directories(h5fortran::h5fortran INTERFACE ${h5fortran_INCLUDE_DIRS})
target_link_libraries(h5fortran::h5fortran INTERFACE ${h5fortran_LIBRARIES} HDF5::HDF5)

# race condition for linking without this
add_dependencies(h5fortran::h5fortran H5FORTRAN)
