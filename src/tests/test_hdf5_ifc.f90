!! unit tests and registration tests of HDF5 OO interface
use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
use, intrinsic:: iso_fortran_env, only: int64, int32, real32, real64, stderr=>error_unit
use, intrinsic:: iso_c_binding, only: c_null_char
use h5fortran, only: hdf5_file, toLower, strip_trailing_null, truncate_string_null, HSIZE_T, h5write, h5read
use test_lt, only : test_readwrite_lt
use test_array, only : test_write_array, test_readwrite_array

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
call testNewHDF5(path)
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


subroutine testNewHDF5(path)
!! create a new HDF5 file
type(hdf5_file) :: h5f
character(*), intent(in) :: path
real(real32), allocatable :: rr1(:)
real(real32) :: rt, r1(4)
integer(int32) :: it, i1(4)
integer(int32), allocatable :: i1t(:)
integer(HSIZE_T), allocatable :: dims(:)

do i = 1,size(i1)
  i1(i) = i
enddo

r1 = i1

call h5f%initialize(path//'/test.h5', ierr, status='new',action='w')
if (ierr /= 0) error stop 'test.h5 open'
!! scalar tests
call h5f%write('/scalar_int', 42_int32, ierr)
if (ierr /= 0) error stop 'write 0-D: int'

call h5f%write('/scalar_real', 42._real32, ierr)
if (ierr /= 0) error stop 'write 0-D: real32'

call h5f%write('/real1',r1, ierr)
if (ierr /= 0) error stop 'write 1-D: real32'

call h5f%write('/ai1', i1, ierr)
if (ierr /= 0) error stop 'write 1-D: scalar int'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

call h5f%initialize(path//'/test.h5', ierr, status='old',action='r')
call h5f%read('/scalar_int', it, ierr)
call h5f%read('/scalar_real', rt, ierr)
if (.not.(rt==it .and. it==42)) then
  write(stderr,*) it,'/=',rt
  error stop 'scalar real / int: not equal 42'
endif

call h5f%shape('/real1',dims, ierr)
allocate(rr1(dims(1)))
call h5f%read('/real1',rr1, ierr)
if (.not.all(r1 == rr1)) error stop 'real 1-D: read does not match write'

call h5f%shape('/ai1',dims, ierr)
allocate(i1t(dims(1)))
call h5f%read('/ai1',i1t, ierr)
if (.not.all(i1==i1t)) error stop 'integer 1-D: read does not match write'

if (.not. h5f%filename == path//'/test.h5') then
  write(stderr,*) h5f%filename // ' mismatch filename'
  error stop
endif

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine testNewHDF5


subroutine testGroup(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path

call h5f%initialize(path//'/test_groups.h5', ierr, status='new',action='rw')
call h5f%write('/test/', ierr)
if (ierr /= 0) error stop 'create group'

call h5f%open('/test', ierr)
if (ierr /= 0) error stop 'open group'
call h5f%write('group3/scalar', 1_int32, ierr)
if (ierr /= 0) error stop 'write 0-D: int32'
call h5f%write('group3/scalar64', 1_int64, ierr)
if (ierr /= 0) error stop 'write 0-D: int64'
call h5f%write('group3/scalar_real', 1._real32, ierr)
if (ierr /= 0) error stop 'write 0-D: real32'
call h5f%close(ierr)
if (ierr /= 0) error stop 'close group'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine testGroup


subroutine test_write_attributes(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path

call h5f%initialize(path//'/test.h5', ierr)

call h5f%writeattr('/nan','note','this is just a little number', ierr)
if (ierr /= 0) error stop 'write attribute string'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine test_write_attributes


subroutine test_string_rw(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path
character(2) :: value
character(1024) :: val1k
character(:), allocatable :: final

call h5f%initialize(path//'/test_string.h5', ierr, status='new', action='rw')
call h5f%write('/little', '42', ierr)

call h5f%read('/little', value, ierr)

if (value /= '42') then
  write(stderr,*) 'string dataset read/write verification failure. Value: '// value
  error stop
endif

!! try reading too much data, then truncating to first C_NULL
call h5f%read('/little', val1k, ierr)
final = truncate_string_null(val1k)

if (len(final) /= 2) then
  write(stderr, *) 'trimming str to c_null did not work, got len() = ', len(final)
  write(stderr, *) iachar(final(3:3))
  error stop
endif

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine test_string_rw


subroutine test_writeExistingVariable(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path
integer :: ierr
character(:),allocatable :: fn

fn = path//'/overwrite.h5'

call h5f%initialize(fn, ierr, status='new',action='w')
if (ierr /= 0) error stop 'write initialize'

call h5f%write('/scalar_int', 42_int32, ierr)
if (ierr /= 0) error stop 'write scalar int'

call h5f%write('/int1d', [42_int32, 1_int32], ierr)
if (ierr /= 0) error stop 'write 1D int'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

call h5f%initialize(fn, ierr, status='old',action='rw')
if (ierr /= 0) error stop 'overwrite initialize'

call h5f%write('/scalar_int', 100_int32, ierr)
if (ierr /= 0) error stop 'overwrite scalar int'

call h5f%write('/int1d', [100_int32, 10_int32], ierr)
if (ierr /= 0) error stop 'overwrite 1D int'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'overwrite finalize'

end subroutine test_writeExistingVariable

end program
