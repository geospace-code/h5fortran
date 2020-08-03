program test_hdf5
!! unit tests and registration tests of HDF5 OO interface
use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit

use h5fortran, only: hdf5_file, h5write, h5read

use test_lt, only : test_readwrite_lt
use test_array, only : test_write_array, test_readwrite_array
use test_scalar, only : test_scalar_rw

implicit none (type, external)

character(:), allocatable :: path
character(256) :: argv
integer :: i,l

call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif
path = trim(argv)
print *, 'test path: ', path

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

call test_writeExistingVariable(path)
print *,'PASSED: write existing variable'


print *,'OK: HDF5 h5fortran library'

contains


subroutine testGroup(path)

type(hdf5_file) :: h5f
character(*), intent(in) :: path

call h5f%initialize(path//'/test_groups.h5', status='new',action='rw')

call h5f%write_group('/test/')

call h5f%open('/test')

call h5f%write('group3/scalar', 1_int32)

call h5f%write('group3/scalar_real', 1._real32)

call h5f%close()

call h5f%finalize()

end subroutine testGroup


subroutine test_writeExistingVariable(path)
type(hdf5_file) :: h5f
character(*), intent(in) :: path
character(:),allocatable :: fn

fn = path//'/overwrite.h5'

call h5f%initialize(fn, status='new',action='w')

call h5f%write('/scalar_int', 42_int32)
call h5f%write('/int1d', [42_int32, 1_int32])

call h5f%finalize()

call h5f%initialize(fn, status='old',action='rw')
call h5f%write('/scalar_int', 100_int32)
call h5f%write('/int1d', [100_int32, 10_int32])
call h5f%finalize()

end subroutine test_writeExistingVariable

end program
