program test_hdf5
!! unit tests and registration tests of HDF5 OO interface
use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit

use h5fortran, only: hdf5_file, h5write, h5read

implicit none (type, external)

call test_group('test_groups.h5')
print *,'OK: HDF5 group'

call test_write_existing('overwrite.h5')
print *,'OK: write existing variable'

contains

subroutine test_group(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h5f

call h5f%open(fn, action='w')

call h5f%write_group('/test/')

call h5f%write('group3/scalar', 1_int32)

call h5f%write('group3/scalar_real', 1._real32)

if(.not. h5f%exist('/test/group3/scalar')) error stop "/test/group3/scalar does not exist: create gorup failed"

call h5f%close()

end subroutine test_group


subroutine test_write_existing(fn)
  type(hdf5_file) :: h5f
  character(*), intent(in) :: fn


call h5f%open(fn, action='w')
call h5f%write('/scalar_int', 42_int32)
call h5f%write('/int1d', [42_int32, 1_int32])
call h5f%close()

call h5f%open(fn, action='r+')
call h5f%write('/scalar_int', 100_int32)
call h5f%write('/int1d', [100_int32, 10_int32])
call h5f%close()

end subroutine test_write_existing

end program
