program fail_unknown_read

use h5fortran, only : hdf5_file
implicit none (type, external)

type(hdf5_file) :: h
complex :: x

call h%open('bad.h5', status='scratch', verbose=.false.)
call h%read('/complex', x)
call h%close()

end program
