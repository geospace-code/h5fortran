program test

use hdf5
use h5fortran
use, intrinsic :: iso_fortran_env, only: stderr=>error_unit

implicit none

type(hdf5_file) :: h
character(*), parameter :: fn = 'test_intent.h5'
integer :: intent(5), exp(5), i

call H5open_f(i)
if(i /= 0) error stop "H5open_f failed [0]"

exp = [H5F_ACC_RDWR_F, H5F_ACC_RDONLY_F, H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_RDWR_F]

!> These are the only two possiblities for the intent of a file
print '(a,i0)', "H5F_ACC_RDONLY_F = ", H5F_ACC_RDONLY_F
print '(a,i0)', "H5F_ACC_RDWR_F = ", H5F_ACC_RDWR_F

call h % open(fn, action='w', debug=.true.)
intent(1) = h % intent()
call h % close()

call h % open(fn, debug=.true.)
intent(2) = h % intent()
call h % close()

call h % open(fn, action='r', debug=.true.)
intent(3) = h % intent()
call h % close()

call h % open(fn, action='r+', debug=.true.)
intent(4) = h % intent()
call h % close()

call h % open(fn, action='rw', debug=.true.)
intent(5) = h % intent()
call h % close()

call H5close_f(i)
if (i /= 0) error stop "H5close() failed"

if (any(intent /= exp)) then
    write(stderr, '(a)') 'test_intent: intent does not match expected values'
    write(stderr, '(5i0)') intent
    write(stderr, '(5i0)') exp
    error stop 'intent test failed'
else
    print '(a)', 'OK: intent matches expected values'
end if

end program
