program nonexistvar

use h5fortran, only : hdf5_file
implicit none

integer :: u
type(hdf5_file) :: h
character(*), parameter :: filename = 'bad.h5'

call h%open(filename, action='r')
call h%read('/not-exist', u)
call h%close()

end program
