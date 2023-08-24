program test_string

use, intrinsic:: iso_fortran_env, only:  stderr=>error_unit
use, intrinsic:: iso_c_binding, only : C_NULL_CHAR

use hdf5, only: H5T_STR_SPACEPAD_F, HSIZE_T
use h5fortran, only: hdf5_file

implicit none

character(*), parameter :: fn='test_string.h5'

call test_write(fn)
print *, "OK: HDF5 string write"

call test_read(fn)
print *,'OK: HDF5 string read'

call test_overwrite(fn)
print *, "OK: string overwrite"

print *,'PASSED: HDF5 string write/read'

contains


subroutine test_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

character(123) :: aux

call h%open(fn, action='w', debug=.true.)

aux = ""
!! compillers like oneAPI need an auxiliary variable not a raw constant
call h%write("/empty", aux)

call h%write('/little', '42')
call h%write('/MySentence', 'this is a little sentence.')
call h%write('/vector_scalar', ['vector scalar'])

call h%write("/1d_empty", [character(1) :: ""])
call h%write("/1d", [character(3) :: "hi", "bye"])
call h%write("/2d", reshape([character(5) :: "one", "two", "three", "four", "five", "six"], [2,3]))

call h%close()

end subroutine test_write


subroutine test_read(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
character(2) :: value
character(1024) :: val1k
character(13) :: vs
integer :: L

integer(HSIZE_T), allocatable :: dims(:)

call h%open(fn, action='r')

value = ""
call h%read('/empty', value)
L = len_trim(value)
print '(L1,1x,i0,1x,a)', trim(value) == C_NULL_CHAR, L, trim(value)
select case (L)
case (1)
  write(stderr, '(a)') 'WARNING: test_string: empty string failure: len_trim should be zero but is 1'
case (0)
  if (value /= '') error stop 'test_string: empty string failure: value = ' // trim(value)
case default
  write(stderr, '(a,i0)') 'test_string: empty string failure: len_trim should be zero but is ', L
  error stop
end select

call h%read('/little', value)

if(len_trim(value) /= 2) then
  write(stderr,'(a,i0,a)') "test_string: read length ", len_trim(value), " /= 2"
  error stop
endif
if (value /= '42') error stop 'test_string:  read/write verification failure. Value: '// value

!> check padding
if (h%get_strpad("/little") /= H5T_STR_SPACEPAD_F) error stop "SPACEPAD expected for /little"

!> longer character than data
call h%read('/little', val1k)

if (len_trim(val1k) /= 2) then
  write(stderr, '(a,i0,/,a)') 'expected character len_trim 2 but got len_trim() = ', len_trim(val1k), val1k
  error stop
endif

!> vector scalar (length 1 vector)
call h%read('/vector_scalar', vs)
if(vs /= "vector scalar") then
  write(stderr, *) "test_string: vector_scalar: expected 'vector_scalar' but got ", vs
  error stop "test_string: vector_scalar"
endif

!> vector
call h%shape("/1d", dims)
if(dims(1) /= 2) error stop "test_string: 1d shape"

!> 2d
call h%shape("/2d", dims)
if(any(dims /= [2, 3])) error stop "test_string: 2d shape"

call h%close()

end subroutine test_read


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
