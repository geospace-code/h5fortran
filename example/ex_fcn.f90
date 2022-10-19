program example1

use h5fortran, only : h5write, h5read
implicit none

character(:), allocatable :: filename
integer :: i32

filename = 'h5fortran_example1.h5'

call h5write(filename, '/x', 123)

call h5read(filename, '/x', i32)
if (i32 /= 123) error stop 'incorrect value read'

print *, 'OK: example 1'

end program
