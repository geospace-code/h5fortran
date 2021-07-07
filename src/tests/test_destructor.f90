program test_destruct
!! test hdf5_file destructor, that should auto-flush and close file
!! if user forgets to %close() file

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use h5fortran, only: hdf5_file
implicit none (type, external)

call test_destructor()
print *, 'OK: destructor'

contains


subroutine test_destructor()
type(hdf5_file) :: h
character(*), parameter :: fn = 'destructor.h5'
integer :: i

block
  type(hdf5_file) :: h
  !! we use block to test destructor is invoked
  call h%open(fn, action="w")
  call h%write('/x', 42)
end block

if(h%is_open) error stop "destructor did not close " // fn

call h%open(fn, action="r")
call h%read("/x", i)
if(i/=42) error stop "destructor did not flush " // fn

end subroutine test_destructor

end program
