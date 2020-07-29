program test_minimal

use hdf5, only : HID_T, HSIZE_T, H5_INTEGER_KIND, h5kind_to_type, h5open_f, h5close_f, h5fclose_f, h5fcreate_f, H5F_ACC_TRUNC_F
use h5lt, only : h5ltmake_dataset_f

implicit none (type, external)

integer :: ierr, p
integer(HID_T) :: lid
character(:), allocatable :: filename
character(256) :: argv
integer :: i, l

call get_command_argument(1, argv, status=i)
if (i /= 0) call get_environment_variable("BINDIR", argv, length=l, status=i)
if (i /= 0 .or. l==0) argv = '.'

filename = trim(argv) // '/test_minimal.h5'
print *, 'test path: ', filename

p = 42

call h5open_f(ierr)
if (ierr /= 0) error stop 'minimal: could not open hdf5 library'

call h5fcreate_f(filename, H5F_ACC_TRUNC_F, lid, ierr)
if (ierr/=0) error stop 'minimal: could not create file'
print *, 'minimal: created '//filename

call h5ltmake_dataset_f(lid, "foo", rank(p), shape(p, kind=HSIZE_T), h5kind_to_type(kind(p),H5_INTEGER_KIND), p, ierr)
if (ierr/=0) error stop 'minimal: could not create dataset foo'
print *, 'minimal: created variable'

call h5fclose_f(lid, ierr)
if (ierr/=0) error stop 'minimal: could not close file'
print *, 'minimal: closed '//filename

call h5close_f(ierr)
if (ierr /= 0) error stop 'could not close hdf5 library'

end program
