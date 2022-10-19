program fail_slice_mismatch
!> test writing wrong size
use h5fortran, only : hdf5_file
implicit none
type(hdf5_file) :: h

call h%open('mismatch.h5', action='w')
call h%write('/int32-1d', [-1,1])
call h%write('/int32-1d', reshape([-1], shape=[1,1]))
call h%close()

end program
