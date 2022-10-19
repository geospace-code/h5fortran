program example2

use h5fortran, only : hdf5_file
implicit none

character(:), allocatable :: filename
integer :: i32

type(hdf5_file) :: h5f

filename = 'h5fortran_example2.h5'

call h5f%open(filename, action='w')
call h5f%write('/x', 123)
call h5f%close()

call h5f%open(filename, action='r')
call h5f%read('/x', i32)
if (i32 /= 123) error stop 'incorrect value read'

print *, 'OK: example 2'

end program
