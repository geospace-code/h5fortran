program fail_slice_mismatch
!> test writing wrong size
use h5fortran, only : hdf5_file
implicit none (type, external)
type(hdf5_file) :: h

call h%initialize('mismatch.h5', status='scratch', verbose=.False.)
call h%write('/int32-1d', [-1,1])
call h%write('/int32-1d', [-1])
call h%finalize()

end program
