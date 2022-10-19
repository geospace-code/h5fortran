program test_scalar

use, intrinsic :: iso_fortran_env, only : real32, real64, int32

use h5fortran, only : hdf5_file

implicit none

call test_simple_write('test_write.h5')
print *, "OK: test simple write"

call test_layout_write('test_layout.h5')
print *, "OK: test layout write"


contains


subroutine test_simple_write(fn)
character(*), intent(in) :: fn

type(hdf5_file) :: h5
integer(int32) :: d0, d1(1), d2(1,2), d3(1,2,3), d4(1,2,3,4), d5(1,2,3,4,5), d6(1,2,3,4,5,6), d7(1,2,3,4,5,6,7)

call h5%open(fn, action="w")

call h5%write("/d0", d0)
call h5%write("/d1", d1)
call h5%write("/d2", d2)
call h5%write("/d3", d3)
call h5%write("/d4", d4)
call h5%write("/d5", d5)
call h5%write("/d6", d6)
call h5%write("/d7", d7)

call h5%close()

end subroutine test_simple_write


subroutine test_layout_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

real(real32), dimension(1,1,1,1,1,1,1) :: d7_32
real(real64), dimension(1,1,1,1,1,1,1) :: d7_64

d7_32 = 42
d7_64 = 42


call h%open(fn, action="w")

call h%write("/compact1d", [1,2,3], compact=.true.)
call h%write("/contig1d", [1,2,3], compact=.false.)

call h%write("/compact0d", 42_int32, compact=.true.)
call h%write("/compact7d_32", d7_32, compact=.true.)
call h%write("/compact7d_64", d7_64, compact=.true.)

call h%write('/compact_r32', 142._real32, compact=.true.)
call h%write('/compact_r64', 142._real64, compact=.true.)
call h%write('/compact_i32', 142_int32, compact=.true.)

call h%close()

end subroutine test_layout_write

end program
