# HDF5 1.14.0 has a bug where basic types are not correct, which causes confusing runtime errors
# check for those and other problems that may arise before build.

include(CheckSourceRuns)

macro(windows_oneapi_hdf5_workaround)

# HDF5 bug #3663 for HDF5 1.14.2, 1.14.3, ...?
# https://github.com/HDFGroup/hdf5/issues/3663
if(WIN32 AND CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
if(HDF5_VERSION MATCHES "1.14.[2-3]")
  message(VERBOSE "HDF5: applying workaround for HDF5 bug #3663 with Intel oneAPI on Windows")
  list(APPEND CMAKE_REQUIRED_LIBRARIES shlwapi)
endif()
endif()

endmacro()


function(hdf5_run_err_diag stderr)

if(DEFINED ENV{CONDA_PREFIX})
  message(WARNING "suggest running 'conda deactivate' and re-running cmake as Conda may be interfering with HDF5 library.")
elseif(WIN32 AND stderr MATCHES "0xc0000135")
  message(STATUS "test run error may be due to missing HDF5 DLLs in PATH
    ${stderr}")
elseif(WIN32 AND stderr MATCHES "0xc0000139")
  message(STATUS "test run error may be due to Windows security policy
    ${stderr}")
else()
  message(WARNING "HDF5 C types failed check")
endif()
endfunction()


function(check_hdf5_c)

if(DEFINED hdf5_c_types_run)
  return()
endif()

set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5)

windows_oneapi_hdf5_workaround()

message(CHECK_START "Checking HDF5 C types")

try_run(hdf5_c_types_run hdf5_c_types_build
SOURCES ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/check_hdf5.c
LINK_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES}
RUN_OUTPUT_STDERR_VARIABLE _stderr
)

if(hdf5_c_types_build AND hdf5_c_types_run EQUAL 0)
  message(CHECK_PASS "passed")
  return()
endif()

message(CHECK_FAIL "failed")

hdf5_run_err_diag(${_stderr})

endfunction(check_hdf5_c)


function(check_hdf5_fortran)

if(DEFINED hdf5_fortran_types_run)
  return()
endif()

set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5)

windows_oneapi_hdf5_workaround()

message(CHECK_START "Checking HDF5 Fortran types")

try_run(hdf5_fortran_types_run hdf5_fortran_types_build
SOURCES ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/check_hdf5.f90
LINK_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES}
RUN_OUTPUT_STDERR_VARIABLE _stderr
)

if(hdf5_fortran_types_build AND hdf5_fortran_types_run EQUAL 0)
  message(CHECK_PASS "passed")
  return()
endif()

message(CHECK_FAIL "failed")

hdf5_run_err_diag(${_stderr})

endfunction(check_hdf5_fortran)


function(check_hdf5)

if(CMAKE_VERSION VERSION_LESS 3.25)
  message(STATUS "HDF5: skipping check for C and Fortran types due to CMake version < 3.25")
  return()
endif()

check_hdf5_c()
check_hdf5_fortran()

endfunction()
