program test_layout

use h5fortran, only : hdf5_file
use hdf5, only : H5D_COMPACT_F

implicit none (type, external)

type(hdf5_file) :: h
character(*), parameter :: fn = 'test_layout.h5'

real :: d7(1,1,1,1,1,1,1)

call h%initialize(fn, status="replace")

call h%write("/compact1d", [1,2,3], compact=.true.)
call h%write("/contig1d", [1,2,3], compact=.false.)

call h%write("/compact0d", 42, compact=.true.)
call h%write("/compact7d", d7, compact=.true.)

if (h%layout("/compact1d") /= H5D_COMPACT_F) error stop "expected compact"
if (.not. h%is_compact("/compact1d")) error stop "1d is_compact fail"

if (.not. h%is_compact("/compact7d")) error stop "7d is_compact fail"
if (.not. h%is_compact("/compact0d")) error stop "0d is_compact fail"

call h%finalize()


end program
