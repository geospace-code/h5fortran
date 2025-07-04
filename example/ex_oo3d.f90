program demo

use h5fortran, only : hdf5_file

implicit none

character(len=*), parameter :: h5file = 'test_oo3d.h5'

type(hdf5_file) :: h5f

real, allocatable :: v3(:, :, :)

allocate(v3(2, 4, 3))

v3 = 0.

call h5f % open(h5file, action='rw', debug=.true.)
call h5f % write('/value1', 123.)
call h5f % close()

!> must always specify action= even if reopening previous file
call h5f % open(h5file, action='rw', comp_lvl = 1, debug=.true.)
call h5f % write('/value2', v3)
call h5f % close()

end program
