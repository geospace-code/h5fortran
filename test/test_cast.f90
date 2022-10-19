program test_cast
!! test HDF5 built-in casting

use h5fortran, only : hdf5_file, &
 H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F, &
 H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE
use, intrinsic :: iso_fortran_env, only : real32, real64, int32, int64

implicit none

character(*), parameter :: fn = 'test_cast.h5'

call test_cast_write(fn)
print "(A)", "OK: cast write"

call test_cast_read(fn)
print "(A)", "OK: cast read"


contains


subroutine test_cast_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

call h%open(fn, action='w')

!> test values
call h%write('/scalar_int32', 42_int32)
call h%write('/scalar_int64', 42_int64)
call h%write('/scalar_real32', 42._real32)
call h%write('/scalar_real64', 42._real64)
call h%write('/1d_real32', [1._real32, 32._real32])
call h%write('/1d_int32', [2_int32, 4_int32])
call h%write('/char', "hello")

call h%close()

end subroutine test_cast_write


subroutine test_cast_read(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

real(real64) :: r64, r1_64(2)
real(real32) :: r32
integer(int32) :: i32
integer(int64) :: i64, i1_64(2)

call h%open(fn, action='r')

!> %class method
if (h%class("/scalar_int32") /= H5T_INTEGER_F) error stop "int32 not integer"
if (h%class("/scalar_int64") /= H5T_INTEGER_F) error stop "int64 not integer"
if (h%class("/scalar_real32") /= H5T_FLOAT_F) error stop "real32 not float"
if (h%class("/scalar_real64") /= H5T_FLOAT_F) error stop "real64 not float"
if (h%class("/char") /= H5T_STRING_F) error stop "char not string"
print *, "OK: class method"

!> %dtype method
if (h%dtype('/scalar_int32') /= H5T_NATIVE_INTEGER) error stop "int32 type"
if (h%dtype("/scalar_int64") /= H5T_STD_I64LE) error stop "int64 type"
if (h%dtype("/scalar_real32") /= H5T_NATIVE_REAL) error stop "real32 type"
if (h%dtype("/scalar_real64") /= H5T_NATIVE_DOUBLE) error stop "real64 type"
if (h%dtype("/char") /= H5T_NATIVE_CHARACTER) error stop "char type"
print *, "OK: dtype method"

!> read casting -- real32 to real64 and int32 to int64
call h%read('/scalar_real32', r64)
if(r64 /= 42) error stop 'scalar cast real32 => real64'
call h%read('/scalar_real64', r32)
if(r32 /= 42) error stop 'scalar cast real64 => real32'
call h%read('/scalar_int32', i64)
if(i64 /= 42) error stop 'scalar cast int32 => int64'
call h%read('/scalar_int64', i32)
if(i32 /= 42) error stop 'scalar cast int64 => int32'
print *, 'PASSED: scalar cast on read'

!> 1D vector read casting -- real to int and int to real
call h%read('/1d_real32', r1_64)
if (.not.all([1., 32.] == r1_64)) error stop '1D cast real32 => real64'
call h%read('/1d_int32', i1_64)
if (.not.all([2, 4] == i1_64)) error stop '1D cast int32 => int64'

call h%close()

print "(A)", "OK: cast"

end subroutine test_cast_read

end program
