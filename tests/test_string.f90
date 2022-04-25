program test_string

use, intrinsic:: iso_fortran_env, only:  stderr=>error_unit

use h5fortran, only : hdf5_file

implicit none (type, external)

character(*), parameter :: path='test_string.h5'

call test_rw(path)
print *, "OK: string read/write"
call test_overwrite(path)

print *,'PASSED: HDF5 string write/read'

contains


subroutine test_rw(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
character(2) :: value
character(80) :: val1k

call h%open(fn, action='w')
call h%write('/little', '42')
call h%write('/MySentence', 'this is a little sentence.')
call h%close()

call h%open(fn, action='r')
call h%read('/little', value)

if(len_trim(value) /= 2) error stop "test_string: read length /= 2"
if (value /= '42') error stop 'test_string:  read/write verification failure. Value: '// value

call h%read('/little', val1k)
if (len_trim(val1k) /= 2) then
  write(stderr, '(a,i0,/,a)') 'expected character len_trim 2 but got len_trim() = ', len_trim(val1k), val1k
  error stop
endif

call h%close()

end subroutine test_rw


subroutine test_overwrite(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
character(2) :: v

call h%open(fn, action='rw')
call h%write('/little', '73')
call h%close()

call h%open(fn, action='r')
call h%read('/little', v)
call h%close()

if (v /= '73') error stop 'test_string:  overwrite string failure. Value: '// v // " /= 73"

end subroutine test_overwrite

end program
