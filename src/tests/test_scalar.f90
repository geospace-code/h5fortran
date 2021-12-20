program scalar_test

use, intrinsic :: iso_fortran_env, only: real32, real64, int32, int64, stderr=>error_unit

use h5fortran, only: hdf5_file
use hdf5, only: HSIZE_T, H5T_NATIVE_INTEGER, H5T_STD_I64LE

implicit none (type, external)

type(hdf5_file) :: h
real(real32), allocatable :: rr1(:)
real(real32) :: rt, r1(4)
real(real64) :: r64

integer(int32) :: it, i1(4)
integer(int32), allocatable :: i1t(:)

integer(int64) :: it_64, i1_64(4)
integer(int64), allocatable :: i1t_64(:)

integer(HSIZE_T), allocatable :: dims(:)
integer :: i
character(*), parameter :: fn = 'test_scalar.h5'

do i = 1,size(i1)
  i1(i) = i
enddo

r1 = i1
i1_64 = i1

!> write
call h%open(fn, action='w')
!> scalar tests
call h%write('/scalar_int32', 42_int32)
call h%write_i32('/scalar_i32', 42_int32)

call h%write('/scalar_int64', 42_int64)
call h%write_i64('/scalar_i64', 42_int64)

call h%write('/scalar_real32', -1._real32)
call h%write_r32("/scalar_r32", -1._real32)

call h%write('/scalar_real64', -1._real64)
call h%write_r64("/scalar_r64", -1._real64)

!> vector
call h%write('/1d_real', r1)
call h%write('/vector_scalar_real', [37.])

!> create then write
call h%create('/1d_int32', H5T_NATIVE_INTEGER, shape(i1, kind=hsize_t))
call h%write('/1d_int32', i1)

call h%create('/1d_int64', H5T_STD_I64LE, shape(i1_64, kind=hsize_t))
call h%write('/1d_int64', i1_64)

print *, 'PASSED: vector write'
!> test rewrite
call h%write('/scalar_real32', 42._real32)
call h%write('/scalar_real64', 42._real64)
call h%write('/scalar_int32', 42_int32)
call h%write('/scalar_int64', 42_int64)
call h%close()

!> read

call h%open(fn, action='r')

call h%read('/scalar_int32', it)
call h%read('/scalar_int64', it_64)

call h%read('/scalar_real32', rt)
if (.not.(rt==it .and. it==42)) then
  write(stderr,*) it,'/=',rt
  error stop 'scalar real / int: not equal 42'
endif
print *, 'PASSED: scalar read/write'

!> read casting -- real32 to real64 and int32 to int64
call h%read('/scalar_real32', r64)
if(r64 /= 42) error stop 'scalar cast real32 => real64'
call h%read('/scalar_real64', rt)
if(rt /= 42) error stop 'scalar cast real64 => real32'
call h%read('/scalar_int32', it_64)
if(it_64 /= 42) error stop 'scalar cast int32 => int64'
call h%read('/scalar_int64', it)
if(it /= 42) error stop 'scalar cast int64 => int32'
print *, 'PASSED: scalar cast on read'

!> read vector length 1 as scalar
call h%shape('/vector_scalar_real', dims)
if (any(dims /= [1])) error stop "vector_scalar: expected vector length 1"

call h%read('/vector_scalar_real', rt)
if(rt/=37) error stop 'vector_scalar: 1d length 1 => scalar'

!> 1D vector read write
call h%shape('/1d_real', dims)
allocate(rr1(dims(1)))
print *, "OK: 1d shape read real32"
call h%read('/1d_real', rr1)
if (.not.all(r1 == rr1)) error stop 'real 1-D: read does not match write'
print *, "OK: 1d read real32"

call h%shape('/1d_int32',dims)
allocate(i1t(dims(1)))
call h%read('/1d_int32',i1t)
if (.not.all(i1==i1t)) error stop 'int32 1-D: read does not match write'

allocate(i1t_64(dims(1)))
call h%read('/1d_int64',i1t_64)
if (.not.all(i1_64==i1t_64)) error stop 'int64 1-D: read does not match write'

print *, 'PASSED: 1D read/write'

!> 1D vector read casting -- real to int and int to real
call h%read('/1d_real', i1t)
if (.not.all(r1==i1t)) error stop '1Dcast real => int32'
call h%read('/1d_real', i1t_64)
if (.not.all(r1==i1t_64)) error stop '1Dcast real => int64'
call h%read('/1d_int32', rr1)
if (.not.all(i1==rr1)) error stop '1D cast int32 => real'
call h%read('/1d_int64', rr1)
if (.not.all(i1_64==rr1)) error stop '1D cast int64 => real'

!> check filename property
if (.not. h%filename == fn) error stop h%filename // ' mismatch filename'

call h%close()

end program
