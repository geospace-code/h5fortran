program test_shape
!! This program shows how HDF5 dimension orders are distinct in different langauges
use h5fortran, only: hdf5_file,hsize_t, is_hdf5
use, intrinsic:: iso_fortran_env, only: real64, stdout=>output_unit, stderr=>error_unit

implicit none (type, external)

type(hdf5_file) :: h
character(*), parameter :: path = 'test_shape.h5'
integer(HSIZE_T), allocatable :: dims(:)

integer :: d2(3,4), d7(2,1,3,4,7,6,5)

call h%initialize(path, status='scratch')
call h%write('/d2', d2)
call h%write('/d7', d7)

call h%shape('/d2', dims)
if (h%ndims('/d2') /= size(dims)) error stop 'rank /= size(dims)'
if (any(dims /= shape(d2))) error stop '2-D: file shape not match variable shape'

call h%shape('/d7', dims)
if (h%ndims('/d7') /= size(dims)) error stop 'rank /= size(dims)'
if (any(dims /= shape(d7))) error stop '7-D: file shape not match variable shape'

call h%finalize()

end program
