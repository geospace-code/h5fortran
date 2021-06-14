program nonexistvar

use h5fortran, only : hdf5_file
implicit none (type, external)

integer :: u
type(hdf5_file) :: h
character(*), parameter :: filename = 'bad.h5'

call h%open(filename, status='scratch', verbose=.false.)
call h%read('/not-exist', u)
call h%close()

end program
