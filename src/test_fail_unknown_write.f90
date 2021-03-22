program fail_unknown_write

use h5fortran, only : hdf5_file
implicit none (type, external)

type(hdf5_file) :: h
complex :: x

x = (1, -1)

call h%initialize('bad.h5',  status='scratch', verbose=.false.)
call h%write('/complex', x)
call h%finalize()

end program
