program nonexistvar

use h5fortran, only : hdf5_file
implicit none

integer :: u, ierr
type(hdf5_file) :: h
character(*), parameter :: filename = 'bad.h5'
logical :: ok

call h%open(filename, action='r', ok=ok)
if (ok) error stop "ERROR: test_fail_nonexist_variable: expected error when opening non-HDF5 file, but got ok=.true."

call h%read('/not-exist', u, ierr=ierr)

call h%close()

if (ierr == 0) error stop "ERROR: test_fail_nonexist_variable: expected error when reading non-existent variable, but got 0"

end program
