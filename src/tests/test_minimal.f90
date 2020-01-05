use hdf5
use h5lt
implicit none
integer :: ierr

call h5open_f(ierr)
if (ierr /= 0) error stop 'could not open hdf5 library'

call h5close_f(ierr)
if (ierr /= 0) error stop 'could not close hdf5 library'

end program