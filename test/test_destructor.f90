program test_destruct
!! test hdf5_file destructor, that should auto-flush and close file
!! if user forgets to %close() file

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use h5fortran, only: hdf5_file

implicit none

character(*), parameter :: fn = "test_destruct.h5"

call test_destructor_write(fn)
print *, 'OK: destructor write'

call test_destructor_read(fn)
print *, 'OK: destructor read'

contains


subroutine test_destructor_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

call h%open(fn, action="w")

call h%write('/x', 42)

!! deliberately omitted %close() to test destructor

end subroutine test_destructor_write


subroutine test_destructor_read(fn)

character(*), intent(in) :: fn

integer :: i
type(hdf5_file) :: h

call h%open(fn, action="r")

call h%read("/x", i)
if(i/=42) error stop "destructor did not flush " // fn

!! deliberately omitted %close() to test destructor

end subroutine test_destructor_read

end program
