program test_hdf5
!! unit tests and registration tests of HDF5 OO interface
use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit

use h5fortran, only: hdf5_file, h5write, h5read

implicit none

call test_group('test_groups.h5')
print *,'OK: HDF5 group'

call test_write_existing('overwrite.h5')
print *,'OK: write existing variable'

contains

subroutine test_group(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

call h % open(fn, action='w', debug=.true.)

call h % create_group('/test')
call h % create_group('test_trailing_slash/')

call h % write('/test/group3/scalar', 1_int32)

call h % write('/test/group3/scalar_real', 1._real32)

if(.not. h % exist('/test/group3/scalar')) error stop "/test/group3/scalar does not exist: create gorup failed"

call h % close()

end subroutine test_group


subroutine test_write_existing(fn)
type(hdf5_file) :: h
character(*), intent(in) :: fn

call h % open(fn, action='w')
call h % write('/scalar_int', 42_int32)
call h % write('/int1d', [42_int32, 1_int32])
call h % close()

call h % open(fn, action='r+')
call h % write('/scalar_int', 100_int32)
call h % write('/int1d', [100_int32, 10_int32])
call h % close()

end subroutine test_write_existing

end program
