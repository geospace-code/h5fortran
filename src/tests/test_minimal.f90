use hdf5, only : HID_T, HSIZE_T, H5_INTEGER_KIND, h5kind_to_type, h5open_f, h5close_f, h5fcreate_f, H5F_ACC_TRUNC_F
use h5lt, only : h5ltmake_dataset_f

implicit none

integer :: ierr, p
integer(HID_T) :: lid
character(:), allocatable :: path
character(256) :: argv
integer :: i,l

call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) argv = './'
path = trim(argv)
print *, 'test path: ', path

p = 42

call h5open_f(ierr)
if (ierr /= 0) error stop 'could not open hdf5 library'

call h5fcreate_f(path // '/junk.h5', H5F_ACC_TRUNC_F, lid, ierr)

call h5ltmake_dataset_f(lid, "foo", rank(p), shape(p, kind=HSIZE_T), h5kind_to_type(kind(p),H5_INTEGER_KIND), p, ierr)

call h5close_f(ierr)
if (ierr /= 0) error stop 'could not close hdf5 library'

end program