program test_deflate_read

use, intrinsic:: iso_fortran_env, only: int32, int64, real32, real64, stderr=>error_unit

use h5fortran, only: hdf5_file

implicit none

character(*), parameter :: fn1='deflate1.h5'
integer, parameter :: N(2) = [50, 1000]


call test_read_deflate(fn1, N)
print *,'OK: HDF5 read deflate'

contains

subroutine test_read_deflate(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N(2)

type(hdf5_file) :: h5f
real(real32), allocatable :: A(:,:)

allocate(A(N(1), N(2)))

call h5f%open(fn, action='r')
call h5f%read('/A', A)
call h5f%read('/noMPI', A)
call h5f%close()

end subroutine test_read_deflate

end program
