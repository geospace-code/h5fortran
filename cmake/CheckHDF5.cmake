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


function(check_hdf5_c)

set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5)

windows_oneapi_hdf5_workaround()

set(_src
[=[
// no newline characters because that trips up CMake check_source_runs() string expansion
#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

int main(void){

if(H5open() != 0){
  fprintf(stderr, "H5open() failed");
  return EXIT_FAILURE;
}

if(H5F_ACC_RDONLY == H5F_ACC_TRUNC || H5F_ACC_RDONLY == H5F_ACC_RDWR){
  fprintf(stderr, "H5F_ACC_RDONLY, H5F_ACC_TRUNC, H5F_ACC_RDWR are not all distinct");
  return EXIT_FAILURE;
}

if(H5close() != 0){
  fprintf(stderr, "H5close() failed");
  return EXIT_FAILURE;
}
printf("OK: HDF5 C type check");
return EXIT_SUCCESS;
}
]=])

check_source_runs(C "${_src}" hdf5_c_types)

if(NOT hdf5_c_types)
  if(DEFINED ENV{CONDA_PREFIX})
    message(WARNING "suggest running 'conda deactivate' and re-running cmake as Conda may be interfering with HDF5 library.")
  endif()
  message(WARNING "HDF5 C types failed check")
endif()

endfunction(check_hdf5_c)


function(check_hdf5_fortran)

set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5)

windows_oneapi_hdf5_workaround()

set(_src [=[
program main

use hdf5

implicit none

integer :: i
integer(HID_T) :: fid

call H5open_f(i)
if(i /= 0) error stop "H5open_f failed [0]"

call H5open_f(i)
if(i /= 0) error stop "H5open_f failed [1]"

print '(a,i0)', "H5F_ACC_RDONLY_F = ", H5F_ACC_RDONLY_F
print '(a,i0)', "H5F_ACC_TRUNC_F = ", H5F_ACC_TRUNC_F
print '(a,i0)', "H5F_ACC_RDWR_F = ", H5F_ACC_RDWR_F

if(H5F_ACC_RDONLY_F == H5F_ACC_TRUNC_F .or. H5F_ACC_RDONLY_F == H5F_ACC_RDWR_F) then
  error stop "H5F_ACC_RDONLY, H5F_ACC_TRUNC, H5F_ACC_RDWR are not all distinct"
endif

call H5close_f(i)
if (i /= 0) error stop "H5close() failed"

print '(a)', "OK: HDF5 C type check"

end program
]=]
)

check_source_runs(Fortran "${_src}" hdf5_fortran_types)

if(NOT hdf5_fortran_types)
  if(DEFINED ENV{CONDA_PREFIX})
    message(WARNING "suggest running 'conda deactivate' and re-running cmake as Conda may be interfering with HDF5 library.")
  endif()
  message(WARNING "HDF5 Fortran types failed check")
endif()

endfunction(check_hdf5_fortran)


function(check_hdf5)

check_hdf5_c()
check_hdf5_fortran()

endfunction()
