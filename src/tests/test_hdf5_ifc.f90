!! unit tests and registration tests of HDF5 OO interface
use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit
use, intrinsic:: iso_c_binding, only: c_null_char
use h5fortran, only: hdf5_file, toLower, strip_trailing_null, truncate_string_null, h5write, h5read
use test_lt, only : test_readwrite_lt
use test_array, only : test_write_array, test_readwrite_array
use test_scalar, only : test_scalar_rw

implicit none

real(real32) :: nan

character(:), allocatable :: path
character(256) :: argv
integer :: i,l,ierr

call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif
path = trim(argv)
print *, 'test path: ', path

nan = ieee_value(1.0, ieee_quiet_nan)

call test_string_rw(path)
print *,'PASSED: HDF5 string write/read'

call test_lowercase()
print *,'PASSED: HDF5 character'
call test_strip_null()
print *,'PASSED: null strip'
call test_scalar_rw(path)
print *,'PASSED: HDF5 scalar real and integer'
call testGroup(path)
print *,'PASSED: HDF5 group'

call test_write_array(path)
print *,'PASSED: HDF5 array write'
call test_readwrite_array(path, ng=69, nn=100, pn=5)
print *,'PASSED: HDF5 array write / read'
call test_readwrite_lt(path)
print *,'PASSED: easy read / write'

call test_write_attributes(path)
print *,'PASSED: HDF5 attributes'

call test_writeExistingVariable(path)
print *,'PASSED: write existing variable'


print *,'OK: HDF5 h5fortran library'

contains


subroutine test_lowercase()

character(*), parameter :: hello = 'HeLl0 Th3rE !>? '
  !! Fortran 2003 allocatable string

if (.not.(toLower(hello)=='hell0 th3re !>? ')) error stop 'error: lowercase conversion'

if (.not.(trim(toLower(hello))=='hell0 th3re !>?')) error stop 'Allocatable lowercase conversion error'

if(.not.all(toLower(['Hi','hI'])==['hi','hi'])) error stop 'error on array conversion'

end subroutine test_lowercase


subroutine test_strip_null()

character(*), parameter :: hello = 'HeLl0 Th3rE !>? '

if (.not.strip_trailing_null(hello // c_null_char) == hello) error stop 'problem stripping trailing null'

end subroutine test_strip_null


subroutine testGroup(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path

call h5f%initialize(path//'/test_groups.h5', ierr, status='new',action='rw')
call h5f%write('/test/', ierr)
if (ierr /= 0) error stop
call h5f%open('/test', ierr)
if (ierr /= 0) error stop
call h5f%write('group3/scalar', 1_int32, ierr)
if (ierr /= 0) error stop
call h5f%write('group3/scalar_real', 1._real32, ierr)
if (ierr /= 0) error stop
call h5f%close(ierr)
if (ierr /= 0) error stop
call h5f%finalize(ierr)
if (ierr /= 0) error stop

end subroutine testGroup


subroutine test_write_attributes(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path

call h5f%initialize(path//'/test.h5', ierr)
if (ierr /= 0) error stop
call h5f%writeattr('/nan','note','this is just a little number', ierr)
if (ierr /= 0) error stop
call h5f%finalize(ierr)
if (ierr /= 0) error stop

end subroutine test_write_attributes


subroutine test_string_rw(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path
character(2) :: value
character(1024) :: val1k
character(:), allocatable :: final

call h5f%initialize(path//'/test_string.h5', ierr, status='new', action='rw')
if (ierr /= 0) error stop
call h5f%write('/little', '42', ierr)
if (ierr /= 0) error stop
call h5f%read('/little', value, ierr)
if (ierr /= 0) error stop
if (value /= '42') then
  write(stderr,*) 'string dataset read/write verification failure. Value: '// value
  error stop
endif

!! try reading too much data, then truncating to first C_NULL
call h5f%read('/little', val1k, ierr)
if (ierr /= 0) error stop
final = truncate_string_null(val1k)

if (len(final) /= 2) then
  write(stderr, *) 'trimming str to c_null did not work, got len() = ', len(final)
  write(stderr, *) iachar(final(3:3))
  error stop
endif

call h5f%finalize(ierr)
if (ierr /= 0) error stop

end subroutine test_string_rw


subroutine test_writeExistingVariable(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path
integer :: ierr
character(:),allocatable :: fn

fn = path//'/overwrite.h5'

call h5f%initialize(fn, ierr, status='new',action='w')
if (ierr /= 0) error stop
call h5f%write('/scalar_int', 42_int32, ierr)
if (ierr /= 0) error stop
call h5f%write('/int1d', [42_int32, 1_int32], ierr)
if (ierr /= 0) error stop
call h5f%finalize(ierr)
if (ierr /= 0) error stop

call h5f%initialize(fn, ierr, status='old',action='rw')
if (ierr /= 0) error stop
call h5f%write('/scalar_int', 100_int32, ierr)
if (ierr /= 0) error stop
call h5f%write('/int1d', [100_int32, 10_int32], ierr)
if (ierr /= 0) error stop
call h5f%finalize(ierr)
if (ierr /= 0) error stop

end subroutine test_writeExistingVariable

end program
