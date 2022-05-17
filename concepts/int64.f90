program main_int64
!! hdf5 fortran interface can also read/write int64, but it's not in the h5lt interface
use hdf5
use, intrinsic :: iso_fortran_env, only : int64

implicit none (type, external)

integer :: ierr

character(:), allocatable :: filename

filename = "int64.h5"

!> initialize fortran interface and file
call h5open_f(ierr)

call write_data(filename)

call verify_data(filename)

call h5close_f(ierr)


contains


subroutine verify_data(filename)

character(*), intent(in) :: filename

integer(hid_t) :: fid
integer :: ierr

call h5fopen_f(filename, h5f_acc_rdonly_f, fid, ierr)

if(read_int64(fid, "/example") /= 12345) error stop "example not match"
if(read_int64(fid, "/big") /= huge(0_int64)) error stop "huge not match"

call h5fclose_f(fid, ierr)

end subroutine verify_data


integer(int64) function read_int64(fid, name) result(i)

integer(hid_t), intent(in) :: fid
character(*), intent(in) :: name

integer(hid_t) ::  dset_id

call h5dopen_f(fid, name, dset_id, ierr)
if (ierr/=0) error stop "dataset not opened: " // name // " in file: " // filename

CALL h5dread_f(dset_id, h5kind_to_type(int64, h5_integer_kind), i, shape(i, hsize_t), ierr)
if (ierr/=0) error stop "dataset not read: " // name // " in file: " // filename

call h5dclose_f(dset_id, ierr)

end function read_int64



subroutine write_data(filename)

character(*), intent(in) :: filename

integer(hid_t) :: fid
integer :: ierr

call h5fcreate_f(filename, h5f_acc_trunc_f, fid, ierr)

call write_int64(fid, 12345_int64, "example")
call write_int64(fid, huge(0_int64), "big") !< 9223372036854775807

call h5fclose_f(fid, ierr)


end subroutine write_data


subroutine write_int64(fid, i, name)

integer(hid_t) :: fid
integer(int64), intent(in) :: i
character(*), intent(in) :: name

integer(hid_t) :: h5_kind_int64, filespace_id, dset_id

h5_kind_int64 = h5kind_to_type(int64, h5_integer_kind)

!> dataspace
call h5screate_f(h5s_scalar_f, filespace_id, ierr)

!> create the dataset.
call h5dcreate_f(fid, name, h5_kind_int64, filespace_id, dset_id, ierr)

!> write data
call h5dwrite_f(dset_id, h5_kind_int64, i, shape(i, hsize_t), ierr)

!> close handles
call h5dclose_f(dset_id, ierr)
call h5sclose_f(filespace_id, ierr)

end subroutine write_int64

end program
