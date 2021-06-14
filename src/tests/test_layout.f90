program test_layout

use, intrinsic :: iso_fortran_env, only : real32, real64
use h5fortran, only : hdf5_file
use hdf5, only : H5D_COMPACT_F

implicit none (type, external)

type(hdf5_file) :: h
character(*), parameter :: fn = 'test_layout.h5'

real(real32), dimension(1,1,1,1,1,1,1) :: d7_32, r7_32
real(real64), dimension(1,1,1,1,1,1,1) :: d7_64, r7_64

real(real32) :: w32, r32
real(real64) :: w64, r64
integer :: wi32, ri32

d7_32 = 42
d7_64 = 42

w32 = 142
w64 = 142
wi32 = 142

call h%open(fn, status="replace", action="write")

call h%write("/compact1d", [1,2,3], compact=.true.)
call h%write("/contig1d", [1,2,3], compact=.false.)

call h%write("/compact0d", 42, compact=.true.)
call h%write("/compact7d_32", d7_32, compact=.true.)
call h%write("/compact7d_64", d7_64, compact=.true.)

call h%write('/compact_r32', w32, compact=.true.)
call h%write('/compact_r64', w64, compact=.true.)
call h%write('/compact_i32', wi32, compact=.true.)

call h%close()

!> read casting
call h%open(fn, status="old", action="read")

call h%read("/compact_r32", r64)
if(r64 /= w32) error stop "read real32 => real64"
call h%read("/compact_r64", r32)
if(r32 /= w64) error stop "read real64 => real32"
call h%read("/compact_i32", r32)
if(r32 /= wi32) error stop "read int32 => real32"
call h%read("/compact_r32", ri32)
if(ri32 /= w32) error stop "real real32 => int32"

call h%read("/compact7d_32", r7_64)
if (any(r7_64 /= d7_32)) error stop "read real32 => real64"

call h%read("/compact7d_64", r7_32)
if (any(r7_32 /= d7_64)) error stop "read real64 => real32"


if (h%layout("/compact1d") /= H5D_COMPACT_F) error stop "expected compact"
if (.not. h%is_compact("/compact1d")) error stop "1d is_compact fail"

if (.not. h%is_compact("/compact7d_32")) error stop "7d is_compact fail"
if (.not. h%is_compact("/compact0d")) error stop "0d is_compact fail"

call h%close()


end program
