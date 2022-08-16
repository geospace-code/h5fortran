submodule (h5fortran:attr_smod) attr_read

use, intrinsic :: iso_c_binding, only : C_CHAR, C_NULL_CHAR, C_F_POINTER

use hdf5, only : H5Aread_f, H5Aget_type_f, &
H5Tis_variable_str_f, H5Tget_class_f, H5Tget_native_type_f, H5Tget_size_f, H5Tget_strpad_f, &
H5T_DIR_ASCEND_F
use h5lt, only: h5ltget_attribute_float_f, h5ltget_attribute_double_f, h5ltget_attribute_int_f

implicit none (type, external)

interface
module subroutine readattr_char_scalar(self, obj_name, attr_name, attr_id, space_id, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, space_id
character(*), intent(inout) :: A
end subroutine

end interface

contains

module procedure readattr_scalar

integer :: ier, attr_class
integer(HID_T) :: attr_id, space_id
integer(HSIZE_T) :: attr_dims(rank(A))
logical :: is_scalar

attr_dims = shape(A, HSIZE_T)

call H5Aopen_by_name_f(self%file_id, obj_name, attr_name, attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name: " // obj_name // ":" // attr_name // ":" // self%filename

call H5Aget_space_f(attr_id, space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aget_space: " // obj_name // ":" // attr_name // ":" // self%filename

call hdf_rank_check(self, obj_name // ":" // attr_name, space_id, rank(A), is_scalar)

call get_obj_class(self, obj_name // ":" // attr_name, attr_id, attr_class)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x, "y")
!! select case doesn't allow H5T_*
if(attr_class == H5T_FLOAT_F .OR. attr_class == H5T_INTEGER_F) then
  select type(A)
  type is (real(real64))
    call H5Aread_f(attr_id, H5T_NATIVE_DOUBLE, A, attr_dims, ier)
  type is (real(real32))
    call H5Aread_f(attr_id, H5T_NATIVE_REAL, A, attr_dims, ier)
  type is (integer(int32))
    call H5Aread_f(attr_id, H5T_NATIVE_INTEGER, A, attr_dims, ier)
  type is (integer(int64))
    call H5Aread_f(attr_id, H5T_STD_I64LE, A, attr_dims, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: numeric dataset ' // obj_name // ':' // attr_name // ' needs real or integer variable'
  end select
elseif(attr_class == H5T_STRING_F) then
  select type(A)
  type is (character(*)) !< kind=c_char too
    call readattr_char_scalar(self, obj_name, attr_name, attr_id, space_id, A)
  class default
    error stop 'ERROR:h5fortran:readattr: string dataset ' // obj_name // ':' // attr_name // ' needs character memory variable'
  end select
else
  error stop "ERROR:h5fortran:readattr: unknown attribute type for " // obj_name // ':' // attr_name
endif

if(ier /= 0) error stop 'ERROR:h5fortran:readattr: reading ' // obj_name // ':' // attr_name // ":" // self%filename

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aclose: " // obj_name // ":" // attr_name // ":" // self%filename

call H5Sclose_f(space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Sclose: " // obj_name // ":" // attr_name // ":" // self%filename

end procedure readattr_scalar




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


end submodule attr_read
