# HDF5 1.14.0 has a bug where basic types are not correct, which causes confusing runtime errors
# check for those and other problems that may arise before build.

include(CheckCSourceRuns)
include(CheckFortranSourceRuns)

function(check_hdf5_c)

set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5)

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

check_c_source_runs("${_src}" hdf5_c_types)

if(NOT hdf5_c_types)
  message(FATAL_ERROR "HDF5 C types failed check")
endif()

endfunction(check_hdf5_c)


function(check_hdf5_fortran)

set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5)

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

check_fortran_source_runs("${_src}" hdf5_fortran_types)

if(NOT hdf5_fortran_types)
  message(FATAL_ERROR "HDF5 Fortran types failed check")
endif()

endfunction(check_hdf5_fortran)


function(check_hdf5)

check_hdf5_c()
check_hdf5_fortran()

endfunction()
