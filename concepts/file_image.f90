module file_image
!! HDF5 file images are an in-RAM fast HDF5 virtual file.
!! there are many caveats to their use and they might never be included in h5fortran

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit, real32, real64, int32
use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_sizeof, c_associated
use hdf5, only :  h5pcreate_f, h5pclose_f, &
 h5pset_file_image_f, h5pget_file_image_f, &
 H5P_FILE_ACCESS_F, hid_t, size_t, &
 h5open_f, h5close_f

!  use h5lt, only : H5LTopen_file_image_f
!! h5ltopen_file_image_f does NOT exist yet as of HDF5 1.12.0. The API was specified for HDF5 1.8.9,
!! but never implemented (yet).

implicit none (type, external)
private
public :: hid_t, create_file_image, close_file_image, write_file_image, read_file_image

contains

subroutine create_file_image(fapl_id)

integer(hid_t), intent(out) :: fapl_id

integer(size_t) :: b_size
integer :: ier
type(C_PTR) :: f_ptr(1:1)

!> open HDF5 library
call h5open_f(ier)
if(ier/=0) error stop 'could not open HDF5 library'

!> create file property ID "fapl_id"
call h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, ier)
if (ier/=0) error stop 'h5pcreate_f'

!> create and check empty buffer
call h5pget_file_image_f(fapl_id, f_ptr, b_size, ier)
if (ier/=0) error stop 'h5pget_file_image: create buffer'
if (b_size /= 0) error stop 'expected empty buffer'

end subroutine create_file_image


subroutine write_file_image(data, fapl_id)
!! polymorphic in future implementation

integer(hid_t), intent(in) :: fapl_id
integer, intent(in) :: data(:)

integer, dimension(1:size(data)), target :: buffer
type(c_ptr) :: f_ptr
integer :: ier

!> write data to HDF5 file image
buffer = data
f_ptr = c_loc(buffer(1))
call h5pset_file_image_f(fapl_id, f_ptr, c_sizeof(buffer), ier)
if(ier/=0) error stop 'h5pset_file_image_f: set buffer size'

end subroutine write_file_image


subroutine read_file_image(buffer, fapl_id)

integer(hid_t), intent(in) :: fapl_id
integer, dimension(:), intent(out), target :: buffer

type(c_ptr) :: f_ptr(1:size(buffer))
integer(size_t) :: b_size
integer :: i, buffer_size

buffer_size = size(buffer) * storage_size(int32) / 8

do i = 1, size(buffer)
  f_ptr(i) = C_LOC(buffer(i))
enddo

call h5pget_file_image_f(fapl_id, f_ptr, b_size, i)
if(i/=0) error stop 'read_file_image: could not read buffer'
if(b_size /= buffer_size) then
  write(stderr, *) "variable size: ", buffer_size, " /= file_image size: ",b_size
  error stop 'read_file_image: variable size /= file_image size'
endif

end subroutine read_file_image


subroutine close_file_image(fapl_id)

integer(hid_t), intent(in) :: fapl_id
integer :: i

!> close file image and library
call h5pclose_f(fapl_id, i)
if (i/=0) error stop 'problem closing file image'

call h5close_f(i)
if (i/=0) error stop 'problem closing HDF5 library'


end subroutine close_file_image


end module file_image


program test_file_image

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use file_image, only : create_file_image, write_file_image, read_file_image, close_file_image, hid_t

implicit none (type, external)

integer(hid_t) :: fapl_id

integer, dimension(1:8) :: in, out
!! arbitrary user data

in = [1,3,7,13,129, -13, 23, 42]

call create_file_image(fapl_id)

call write_file_image(in, fapl_id)

call read_file_image(out, fapl_id)

call close_file_image(fapl_id)

if (any(in /= out)) then
  write(stderr,*) 'in=',in
  write(stderr,*) 'out=',out
  error stop 'output data does not match input data'
endif

print *, 'OK: file image prototype'

end program
