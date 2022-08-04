submodule (h5fortran:attr_smod) attr_read

use, intrinsic :: iso_c_binding, only : C_CHAR, C_NULL_CHAR, C_F_POINTER

use hdf5, only : H5Aread_f, H5Aget_type_f, &
H5Tis_variable_str_f, H5Tget_class_f, H5Tget_native_type_f, H5Tget_size_f, H5Tget_strpad_f, &
H5T_DIR_ASCEND_F
use h5lt, only: h5ltget_attribute_float_f, h5ltget_attribute_double_f, h5ltget_attribute_int_f

implicit none (type, external)

contains

module procedure readattr_scalar

integer(HSIZE_T) :: attr_dims(0)
integer(HID_T) :: attr_id, space_id
integer :: attr_class, ier
logical :: is_scalar, attr_exists

if(.not.self%is_open()) error stop 'ERROR:h5fortran:attr_read: file handle is not open'

call H5Aexists_by_name_f(self%file_id, obj_name, attr_name, attr_exists, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attr_read:H5Aexists_by_name " // obj_name // ":" // attr_name // " : " // self%filename
if(.not.attr_exists) then
  error stop 'ERROR:h5fortran:attr_read: attribute not exist: ' // obj_name // ":" // attr_name // " : " // self%filename
endif

call H5Aopen_by_name_f(self%file_id, obj_name, attr_name, attr_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:readattr:H5Aopen ' // obj_name // ":" // attr_name // " : " // self%filename

call H5Aget_space_f(attr_id, space_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:readattr:H5Aget_space ' // obj_name // ":" // attr_name

call attr_rank_check(self, obj_name, attr_name, space_id, rank(A), is_scalar)

call get_attr_class(self, obj_name, attr_name, attr_class, attr_id)

!> cast the dataset read from disk to the variable type presented by user h5f%readattr("/my_dataset", x, "y")
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
if(attr_class == H5T_FLOAT_F) then
  select type(A)
  type is (real(real64))
    call H5Aread_f(attr_id, H5T_NATIVE_DOUBLE, A, attr_dims, ier)
  type is (real(real32))
    call H5Aread_f(attr_id, H5T_NATIVE_REAL, A, attr_dims, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: real disk dataset ' // obj_name // ':' // attr_name // ' needs real memory variable'
  end select
elseif(attr_class == H5T_INTEGER_F) then
  select type(A)
  type is (integer(int32))
    call H5Aread_f(attr_id, H5T_NATIVE_INTEGER, A, attr_dims, ier)
  type is (integer(int64))
    call H5Aread_f(attr_id, H5T_STD_I64LE, A, attr_dims, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: integer disk dataset ' // obj_name // ':' // attr_name // ' needs integer memory variable'
  end select
elseif(attr_class == H5T_STRING_F) then
  select type(A)
  type is (character(*)) !< kind=c_char too
    call readattr_char(self, obj_name, attr_name, attr_id, space_id, A)
  class default
    error stop 'ERROR:h5fortran:readattr: string dataset ' // obj_name // ':' // attr_name // ' needs character memory variable'
  end select
else
  error stop 'ERROR:h5fortran:attr_read: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'ERROR:h5fortran:readattr: reading ' // obj_name // ':' // attr_name // ' from ' // self%filename

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr_scalar:H5Aclose: " // obj_name // ':' // attr_name // " in " // self%filename

call H5Sclose_f(space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr_scalar:H5Sclose: " // obj_name // ':' // attr_name // " in " // self%filename

end procedure readattr_scalar


subroutine readattr_char(self, obj_name, attr_name, attr_id, space_id, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, space_id
character(*), intent(inout) :: A

integer(HSIZE_T) :: attr_dims(1), maxdims(1)
integer(HID_T) :: type_id
integer :: ier, i, L, drank
integer(HSIZE_T) :: dsize

logical :: vstatus

!> variable length string
TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
TYPE(C_PTR) :: f_ptr

CHARACTER(10000, kind=c_char), POINTER :: cstr !< arbitrary maximum variable length string
CHARACTER(:), DIMENSION(:), ALLOCATABLE, TARGET :: buf_char !< fixed length read

L = len(A)

call H5Aget_type_f(attr_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_type " // obj_name // ":" // attr_name // " " // self%filename

call H5Tget_size_f(type_id, dsize, ier) !< only for non-variable
if(ier/=0) error stop "ERROR:h5fortran:attr_read:H5Tget_size " // obj_name // ":" // attr_name // " " // self%filename

CALL H5Sget_simple_extent_ndims_f(space_id, drank, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Sget_simple_extent_ndims " // obj_name // ":" // attr_name

if(drank > 0) then
  CALL H5Sget_simple_extent_dims_f(space_id, attr_dims, maxdims, ier)
  if(ier /= drank) error stop "ERROR:h5fortran:readattr:H5Sget_simple_extent_dims " // obj_name // ":" // attr_name
else
  attr_dims(1) = 1
endif

call H5Tis_variable_str_f(type_id, vstatus, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Tis_variable_str " // obj_name // ":" // attr_name

if(vstatus) then
  allocate(cbuf(1:attr_dims(1)))
  f_ptr = C_LOC(cbuf(1))

  call H5Aread_f(attr_id, type_id, f_ptr, ier)
  if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // obj_name // ":" // attr_name

  call C_F_POINTER(cbuf(1), cstr)

  i = index(cstr, c_null_char) - 1
  if (i == -1) i = len_trim(cstr)
  if(i > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr_vlen: buffer too small: ", i, " > ", L, obj_name // ":" // attr_name
    error stop
  endif

  A = cstr(:i)
else
  if(dsize > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", dsize, " > ", L, obj_name // ":" // attr_name
    error stop
  endif

  allocate(character(dsize) :: buf_char(attr_dims(1)))

  f_ptr = C_LOC(buf_char(1)(1:1))

  call H5Aread_f(attr_id, type_id, f_ptr, ier)
  if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // obj_name // ":" // attr_name

  i = index(buf_char(1), c_null_char) - 1
  if (i == -1) i = len_trim(buf_char(1))

  A = buf_char(1)(:i)

endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name // ":" // attr_name

end subroutine readattr_char


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


subroutine get_attr_class(self, obj_name, attr_name, class, attr_id, size_bytes, pad_type)
!! get the attribute class (integer, float, string, ...)
!! {H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F}
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer, intent(out) :: class
integer(HID_T), intent(in) :: attr_id
integer(SIZE_T), intent(out), optional :: size_bytes
integer, intent(out), optional :: pad_type

integer :: ierr
integer(HID_T) :: type_id, native_type_id

call H5Aget_type_f(attr_id, type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: type_id ' // obj_name // ":" // attr_name // " " // self%filename

call H5Tget_native_type_f(type_id, H5T_DIR_ASCEND_F, native_type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: native_type_id ' // obj_name // ":" // attr_name

!> compose datatype inferred
call H5Tget_class_f(native_type_id, class, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: class ' // obj_name // ":" // attr_name

if(present(size_bytes)) then
  call H5Tget_size_f(native_type_id, size_bytes, ierr)
  if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: byte size ' // obj_name // ":" // attr_name
endif

if(present(pad_type)) then
  if(class /= H5T_STRING_F) error stop "ERROR:h5fortran:get_attr_class: pad_type only for string"

  call H5Tget_strpad_f(type_id, pad_type, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:get_attr_class:h5tget_strpad " // obj_name // ":" // attr_name
endif

!> close to avoid memory leaks
call H5Tclose_f(native_type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: closing native dtype ' // obj_name // ":" // attr_name

call H5Tclose_f(type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: closing dtype ' // obj_name // ":" // attr_name

end subroutine get_attr_class


end submodule attr_read
