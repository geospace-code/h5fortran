program test_minimal

use hdf5, only : HID_T, HSIZE_T, H5_INTEGER_KIND, h5kind_to_type, h5open_f, h5close_f, h5fclose_f, h5fcreate_f, H5F_ACC_TRUNC_F
use h5lt, only : h5ltmake_dataset_f

implicit none

integer :: i, p
integer(HID_T) :: lid
character(*), parameter :: filename='test_minimal.h5'

p = 42

call h5open_f(i)
if (i /= 0) error stop 'minimal: could not open hdf5 library'

call h5fcreate_f(filename, H5F_ACC_TRUNC_F, lid, i)
if (i/=0) error stop 'minimal: could not create file'
print *, 'minimal: created '//filename

call h5ltmake_dataset_f(lid, "A", rank(p), shape(p, kind=HSIZE_T), h5kind_to_type(kind(p),H5_INTEGER_KIND), p, i)
if (i/=0) error stop 'minimal: could not create dataset A'
print *, 'minimal: created variable'

call h5fclose_f(lid, i)
if (i/=0) error stop 'minimal: could not close file'
print *, 'minimal: closed '//filename

call h5close_f(i)
if (i /= 0) error stop 'could not close hdf5 library'

! this is a Fortran-standard way to delete files
open(newunit=i, file=filename)
close(i, status='delete')

end program
