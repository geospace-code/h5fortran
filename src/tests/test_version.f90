program test_version
!! tests that HDF5 library version is available

use h5fortran, only : hdf5version

implicit none (type, external)

integer :: v(3)

v = hdf5version()

print '(i0,a1,i0,a1,i0)', v(1), '.', v(2), '.', v(3)

end program
