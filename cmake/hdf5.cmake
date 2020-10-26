# don't enclose this all in "if(NOT DEFINED HDF5OK)" because CMake intermittantly doesn't cache needed HDF5 variables.

if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.19)
  # make missing imported targets fail immediately
  cmake_policy(SET CMP0111 NEW)
endif()

if(hdf5_external)
  include(${CMAKE_CURRENT_LIST_DIR}/build_hdf5.cmake)
  return()
endif()

set(HDF5_USE_STATIC_LIBRARIES true)
if(MSVC)
# Intel HDF5 for Windows needs this.
  set(HDF5_NO_FIND_PACKAGE_CONFIG_FILE true)
  set(HDF5_USE_STATIC_LIBRARIES false)
endif()

if(NOT hdf5_external)
  find_package(HDF5 COMPONENTS Fortran HL)
endif()

if(NOT HDF5_FOUND)
  include(${CMAKE_CURRENT_LIST_DIR}/build_hdf5.cmake)
  return()
endif()

# --- library patch
set(HDF5_LIBRARIES ${HDF5_Fortran_HL_LIBRARIES} ${HDF5_Fortran_LIBRARIES} ${HDF5_LIBRARIES})

# hdf5hl_fortran <=> hdf5_hl_fortran patch
list(GET HDF5_LIBRARIES 0 _hlf)
if(NOT _hlf)
  list(REMOVE_AT HDF5_LIBRARIES 0)
  find_library(_hlf NAMES hdf5_hl_fortran REQUIRED)
  if(_hlf)
    list(PREPEND HDF5_LIBRARIES ${_hlf})
  else()
    message(FATAL_ERROR "hdf5_hl_fortran patch failed")
  endif()
endif()

# --- include patch
if(HDF5_Fortran_INCLUDE_DIRS)
  list(APPEND HDF5_INCLUDE_DIRS ${HDF5_Fortran_INCLUDE_DIRS})
endif()
# this is because HDF5's CONFIG files are incorrect (at least for 1.10.5)
find_path(HDF5_MODULE_DIR NAMES hdf5.mod HINTS ${HDF5_INCLUDE_DIRS} PATH_SUFFIXES static)
if(HDF5_MODULE_DIR)
  list(APPEND HDF5_INCLUDE_DIRS ${HDF5_MODULE_DIR})
  set(SZIP_ROOT "${HDF5_MODULE_DIR}/..;${HDF5_MODULE_DIR}/../..")
  set(ZLIB_ROOT "${HDF5_MODULE_DIR}/..;${HDF5_MODULE_DIR}/../..")
endif()
list(REMOVE_DUPLICATES HDF5_INCLUDE_DIRS)

if(NOT DEFINED HDF5OK)
  message(STATUS "HDF5 include: ${HDF5_INCLUDE_DIRS}")
  message(STATUS "HDF5 library: ${HDF5_LIBRARIES}")

  if(MSVC)
    include(${CMAKE_CURRENT_LIST_DIR}/win32_hdf5.cmake)
    win32_hdf5_env()
  endif(MSVC)
endif()

# --- configure time checks
# these checks avoid messy, confusing errors at build time
# cmake_required_* set again here to include zlib etc.

# this is here for check_symbol
set(CMAKE_REQUIRED_INCLUDES ${HDF5_INCLUDE_DIRS})

include(CheckSymbolExists)
check_symbol_exists(H5_HAVE_FILTER_SZIP H5pubconf.h use_szip)
check_symbol_exists(H5_HAVE_FILTER_DEFLATE H5pubconf.h use_zlib)

if(use_zlib)
  find_package(ZLIB REQUIRED)

  if(use_szip)
    # Szip even though not directly used because if system static links libhdf5 with szip,
    # our builds will fail if we don't also link szip.
    find_package(SZIP REQUIRED)
    set(CMAKE_REQUIRED_LIBRARIES ${HDF5_LIBRARIES} SZIP::SZIP ZLIB::ZLIB ${CMAKE_DL_LIBS})
  else()
    set(CMAKE_REQUIRED_LIBRARIES ${HDF5_LIBRARIES} ZLIB::ZLIB ${CMAKE_DL_LIBS})
  endif()
endif()

set(THREADS_PREFER_PTHREAD_FLAG true)
find_package(Threads)
if(Threads_FOUND)
  list(APPEND CMAKE_REQUIRED_LIBRARIES Threads::Threads)
endif()

include(CheckFortranSourceCompiles)
set(_code "program test_minimal
use hdf5, only : h5open_f, h5close_f
use h5lt, only : h5ltmake_dataset_f
implicit none (type, external)
integer :: i
call h5open_f(i)
if (i /= 0) error stop 'could not open hdf5 library'
call h5close_f(i)
if (i /= 0) error stop
end")
check_fortran_source_compiles(${_code} HDF5_compiles_ok SRC_EXT f90)

include(CheckFortranSourceRuns)
if(HDF5_compiles_ok)
check_fortran_source_runs(${_code} HDF5_runs_ok SRC_EXT f90)
endif(HDF5_compiles_ok)

if(HDF5_compiles_ok AND (MSVC OR HDF5_runs_ok))
  # MSVC check_fortran_source_runs needs to be set to PROJECT_BINARY_DIR.
  # may require vendoring, so we just do this workaround for now for MSVC.
  set(HDF5OK true CACHE BOOL "HDF5 library compiles and run OK")
  # --- imported target
  # NOTE: this is coming to CMake 3.19 FindHDF5, but their alpha didn't work for Intel Windows.
  add_library(HDF5::HDF5 INTERFACE IMPORTED GLOBAL)
  target_include_directories(HDF5::HDF5 INTERFACE "${HDF5_INCLUDE_DIRS}")
  target_link_libraries(HDF5::HDF5 INTERFACE "${HDF5_LIBRARIES}")
  target_compile_definitions(HDF5::HDF5 INTERFACE "${HDF5_DEFINITIONS}")

  if(use_zlib)
    target_link_libraries(HDF5::HDF5 INTERFACE ZLIB::ZLIB)
  endif()
  if(use_szip)
    target_link_libraries(HDF5::HDF5 INTERFACE SZIP::SZIP)
  endif()

  if(Threads_FOUND)
    target_link_libraries(HDF5::HDF5 INTERFACE Threads::Threads)
  endif()

  target_link_libraries(HDF5::HDF5 INTERFACE ${CMAKE_DL_LIBS})

  if(UNIX)
    target_link_libraries(HDF5::HDF5 INTERFACE m)
  endif()
else()
  unset(HDF5_FOUND)
  unset(HDF5_INCLUDE_DIRS)
  unset(HDF5_LIBRARIES)
  unset(HDF5_DEFINITIONS)
  unset(ZLIB::ZLIB)
  unset(SZIP::SZIP)
  include(${CMAKE_CURRENT_LIST_DIR}/build_hdf5.cmake)
endif()
