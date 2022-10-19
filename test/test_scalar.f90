program test_scalar

use, intrinsic :: iso_fortran_env, only: real32, real64, int32, int64, stderr=>error_unit

use hdf5, only: HSIZE_T, H5T_NATIVE_INTEGER, H5T_STD_I64LE
use h5fortran, only: hdf5_file

implicit none

character(*), parameter :: fn = 'test_scalar.h5'

call test_scalar_write(fn)
print *, 'OK: scalar and vector: write and rewrite'

call test_scalar_read(fN)
print *, 'OK: scalar and vector: read'

contains

subroutine test_scalar_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

real(real32) :: r1(4)
integer(int32) :: i1(4)
integer(int64) :: i1_64(4)

integer :: i

do i = 1,size(i1)
  i1(i) = i
enddo

r1 = i1
i1_64 = i1

!> write
call h%open(fn, action='w')
!> scalar tests
call h%write('/scalar_int32', 42_int32)
call h%write('/scalar_int64', 42_int64)
call h%write('/scalar_real32', -1._real32)
call h%write('/scalar_real64', -1._real64)

!> vector
call h%write('/1d_real', r1)
call h%write('/vector_scalar_real', [37.])

!> create then write
call h%create('/1d_int32', H5T_NATIVE_INTEGER, dset_dims=shape(i1))
call h%write('/1d_int32', i1)

call h%create('/1d_int64', H5T_STD_I64LE, dset_dims=shape(i1_64))
call h%write('/1d_int64', i1_64)

!> test rewrite
call h%write('/scalar_real32', 42._real32)
call h%write('/scalar_real64', 42._real64)
call h%write('/scalar_int32', 42_int32)
call h%write('/scalar_int64', 42_int64)
call h%close()

end subroutine test_scalar_write


subroutine test_scalar_read(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

real(real32) :: rt, r1(4)
integer(int32) :: i, it, i1(4)
integer(int32), allocatable :: i1t(:)
integer(int64) :: it_64, i1_64(4)
integer(int64), allocatable :: i1t_64(:)
real(real32), allocatable :: rr1_32(:)
integer(HSIZE_T), allocatable :: dims(:)

!> test data
do i = 1,size(i1)
  i1(i) = i
enddo

r1 = i1
i1_64 = i1

!> read

call h%open(fn, action='r')

call h%read('/scalar_int32', it)
call h%read('/scalar_int64', it_64)

call h%read('/scalar_real32', rt)
if (.not.(rt==it .and. it==42)) then
  write(stderr,*) it,'/=',rt
  error stop 'scalar real / int: not equal 42'
endif

!> read vector length 1 as scalar
call h%shape('/vector_scalar_real', dims)
if (any(dims /= [1])) error stop "vector_scalar: expected vector length 1"

call h%read('/vector_scalar_real', rt)
if(rt /= 37) error stop 'vector_scalar: 1d length 1 => scalar'

!> 1D vector read
call h%shape('/1d_real', dims)
allocate(rr1_32(dims(1)))

call h%read('/1d_real', rr1_32)
if (.not.all(r1 == rr1_32)) error stop 'real32 1-D: read does not match write'

call h%shape('/1d_int32',dims)
allocate(i1t(dims(1)))
call h%read('/1d_int32', i1t)
if (.not.all(i1==i1t)) error stop 'int32 1-D: read does not match write'

allocate(i1t_64(dims(1)))
call h%read('/1d_int64', i1t_64)
if (.not.all(i1_64==i1t_64)) error stop 'int64 1-D: read does not match write'

!> check filename property
if (.not. h%filename == fn) error stop h%filename // ' mismatch filename'

call h%close()

end subroutine test_scalar_read

end program
