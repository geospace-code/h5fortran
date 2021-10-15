program test_string

use, intrinsic:: iso_fortran_env, only:  stderr=>error_unit

use h5fortran, only : hdf5_file

implicit none (type, external)

type(hdf5_file) :: h
character(2) :: value
character(1024) :: val1k

character(*), parameter :: path='test_string.h5'

call h%open(path, action='w')

call h%write('/little', '42')
call h%write_char('/little_char', '42')
call h%write('/MySentence', 'this is a little sentence.')

call h%close()

call h%open(path, action='r')
call h%read('/little', value)

if (value /= '42') error stop 'test_string:  read/write verification failure. Value: '// value

print *,'test_string_rw: longer character than data'
call h%read('/little', val1k)


if (len_trim(val1k) /= 2) then
  write(stderr, *) 'expected character len_trim 2 but got len_trim() = ', len_trim(val1k)
  error stop
endif

call h%close()

print *,'PASSED: HDF5 string write/read'

end program
