program test_hdf5
!! unit tests and registration tests of HDF5 OO interface
use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit

use h5fortran, only: hdf5_file, h5write, h5read

implicit none (type, external)

call testGroup()
print *,'PASSED: HDF5 group'

call test_writeExistingVariable()
print *,'PASSED: write existing variable'

contains

subroutine testGroup()

type(hdf5_file) :: h5f

call h5f%open('test_groups.h5', action='w')

call h5f%write_group('/test/')

call h5f%open_group('/test')

call h5f%write('group3/scalar', 1_int32)

call h5f%write('group3/scalar_real', 1._real32)

call h5f%close_group()

if(.not. h5f%exist('/test/group3/scalar')) error stop "/test/group3/scalar does not exist: create gorup failed"

call h5f%close()

end subroutine testGroup


subroutine test_writeExistingVariable()
type(hdf5_file) :: h5f
character(*), parameter :: fn = 'overwrite.h5'

call h5f%open(fn, action='w')
call h5f%write('/scalar_int', 42_int32)
call h5f%write('/int1d', [42_int32, 1_int32])
call h5f%close()

call h5f%open(fn, action='r+')
call h5f%write('/scalar_int', 100_int32)
call h5f%write('/int1d', [100_int32, 10_int32])
call h5f%close()

end subroutine test_writeExistingVariable

end program
