submodule (h5fortran:attr_smod) attr_read

use, intrinsic :: iso_c_binding, only : C_CHAR, C_NULL_CHAR, C_F_POINTER

use hdf5, only : H5Aread_f, H5Aget_type_f, &
H5Tis_variable_str_f, H5Tget_class_f, H5Tget_native_type_f, H5Tget_size_f, H5Tget_strpad_f, &
H5T_DIR_ASCEND_F
use h5lt, only: h5ltget_attribute_float_f, h5ltget_attribute_double_f, h5ltget_attribute_int_f

implicit none (type, external)

interface readattr_char
procedure readattr_char_scalar, readattr_char1, readattr_char2, readattr_char3, readattr_char4, &
  readattr_char5, readattr_char6, readattr_char7
end interface

interface
module subroutine readattr_char_scalar(self, obj_name, attr_name, attr_id, space_id, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, space_id
character(*), intent(inout) :: A
end subroutine

module subroutine open_attr_char(self, obj_name, attr_name, attr_id, space_id, type_id, attr_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, space_id
integer(HID_T), intent(out) :: type_id, attr_dims(:)
end subroutine
end interface

contains

module procedure readattr_scalar
include 'attr_read.inc'
end procedure

module procedure readattr_1d
include 'attr_read.inc'
end procedure

module procedure readattr_2d
include 'attr_read.inc'
end procedure

module procedure readattr_3d
include 'attr_read.inc'
end procedure

module procedure readattr_4d
include 'attr_read.inc'
end procedure

module procedure readattr_5d
include 'attr_read.inc'
end procedure

module procedure readattr_6d
include 'attr_read.inc'
end procedure

module procedure readattr_7d
include 'attr_read.inc'
end procedure

module procedure lt0readattr

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr(obj_name, attr, A)
call h%close()

end procedure lt0readattr


module procedure lt1readattr

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr(obj_name, attr, A)
call h%close()

end procedure lt1readattr


subroutine readattr_char1(self, obj_name, attr_name, attr_id, space_id, A)
character(*), intent(inout), dimension(:) :: A
include 'attr_read_char.inc'
end subroutine

subroutine readattr_char2(self, obj_name, attr_name, attr_id, space_id, A)
character(*), intent(inout), dimension(:,:) :: A
include 'attr_read_char.inc'
end subroutine

subroutine readattr_char3(self, obj_name, attr_name, attr_id, space_id, A)
character(*), intent(inout), dimension(:,:,:) :: A
include 'attr_read_char.inc'
end subroutine

subroutine readattr_char4(self, obj_name, attr_name, attr_id, space_id, A)
character(*), intent(inout), dimension(:,:,:,:) :: A
include 'attr_read_char.inc'
end subroutine

subroutine readattr_char5(self, obj_name, attr_name, attr_id, space_id, A)
character(*), intent(inout), dimension(:,:,:,:,:) :: A
include 'attr_read_char.inc'
end subroutine

subroutine readattr_char6(self, obj_name, attr_name, attr_id, space_id, A)
character(*), intent(inout), dimension(:,:,:,:,:,:) :: A
include 'attr_read_char.inc'
end subroutine

subroutine readattr_char7(self, obj_name, attr_name, attr_id, space_id, A)
character(*), intent(inout), dimension(:,:,:,:,:,:,:) :: A
include 'attr_read_char.inc'
end subroutine

end submodule attr_read
