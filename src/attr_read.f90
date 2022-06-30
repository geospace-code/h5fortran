submodule (h5fortran) attr_read

use, intrinsic :: iso_c_binding, only : C_CHAR, C_NULL_CHAR, C_F_POINTER

use hdf5, only : H5Aexists_by_name_f, H5Aopen_by_name_f, H5Aread_f, H5Aclose_f, H5Aget_info_f, H5Aget_type_f, &
H5Tclose_f, H5Tis_variable_str_f, H5Tget_class_f, H5Tget_native_type_f, H5Tget_size_f, H5Tget_strpad_f, &
H5T_DIR_ASCEND_F
use h5lt, only: h5ltget_attribute_float_f, h5ltget_attribute_double_f, h5ltget_attribute_int_f, &
h5ltget_attribute_ndims_f, h5ltget_attribute_info_f

implicit none (type, external)

contains


module procedure readattr_char
!! NOTE: HDF5 character attributes are scalar.
integer(HID_T) :: type_id, attr_id
integer :: ier, i, L
integer(HSIZE_T) :: dsize

logical :: vstatus, f_corder_valid
integer :: corder, cset, attr_class

!> variable length string
TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
TYPE(C_PTR) :: f_ptr

CHARACTER(10000, kind=c_char), POINTER :: cstr !< arbitrary maximum variable length string
character(:), allocatable :: buf_char !< fixed length read

integer(HSIZE_T) :: dims(0)

L = len(A)

call attr_shape_check(self, dname, attr, shape(A))

call H5Aopen_by_name_f(self%file_id, dname, attr, attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name_f failed: " // dname // ":" // attr

call get_attr_class(self, dname, attr, attr_class, attr_id)

if(attr_class /= H5T_STRING_F) then
  error stop 'ERROR:h5fortran:readattr: character disk attribute ' // dname // ':' // attr // ' needs character memory variable'
endif

call H5Aget_type_f(attr_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_type " // dname
call H5Aget_info_f(attr_id, f_corder_valid, corder, cset, dsize, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_info " // dname

call H5Tis_variable_str_f(type_id, vstatus, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Tis_variable_str " // trim(dname)

if(vstatus) then
  allocate(cbuf(1:dsize))
  f_ptr = C_LOC(cbuf(1))

  call H5Aread_f(attr_id, type_id, f_ptr, ier)
  if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // dname // ":" // attr

  call C_F_POINTER(cbuf(1), cstr)

  i = index(cstr, c_null_char) - 1
  if (i == -1) i = len_trim(cstr)
  if(i > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr_vlen: buffer too small: ", i, " > ", L, dname // ":" // attr
    error stop
  endif

  A = cstr(:i)
else
  if(dsize > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", dsize, " > ", L, dname // ":" // attr
    error stop
  endif

  allocate(character(dsize) :: buf_char)

  call H5Aread_f(attr_id, type_id, buf_char, dims, ier)
  if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // dname // ":" // attr

  i = index(buf_char, c_null_char) - 1
  if (i == -1) i = len_trim(buf_char)

  A = buf_char(:i)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // dname // ":" // attr

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aclose " // dname // ":" // attr

end procedure readattr_char


module procedure readattr_num
!! NOTE: HDF5 has 1D vector attributes for integer, float and double.
integer :: ier, attr_class

call attr_shape_check(self, dname, attr, shape(A))

call get_attr_class(self, dname, attr, attr_class)

if(attr_class == H5T_FLOAT_F) then
  select type(A)
  type is (real(real32))
    call h5ltget_attribute_float_f(self%file_id, dname, attr, A, ier)
  type is (real(real64))
    call h5ltget_attribute_double_f(self%file_id, dname, attr, A, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: real disk attribute ' // dname // ':' // attr // ' needs real memory variable'
  end select
elseif(attr_class == H5T_INTEGER_F) then
  select type(A)
  type is (integer(int32))
    call h5ltget_attribute_int_f(self%file_id, dname, attr, A, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: integer disk attribute ' // dname // ':' // attr // ' needs integer memory variable'
  end select
else
  error stop "ERROR:h5fortran:readattr_num: unknown attribute type for " // dname // ':' // attr
endif

if (ier /= 0) error stop "ERROR:h5fortran:readattr_num: " // dname // " in " // self%filename

end procedure readattr_num


module procedure readattr_char_lt

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr_char(dname, attr, A)
call h%close()

end procedure readattr_char_lt


module procedure readattr_num_lt

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr_num(dname, attr, A)
call h%close()

end procedure readattr_num_lt


subroutine attr_shape_check(self, dset_name, attr_name, dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dset_name, attr_name
integer, intent(in) :: dims(:)

integer :: attr_rank, attr_type, ierr
integer(size_t) :: attr_bytes
integer(HSIZE_T), dimension(size(dims)):: attr_dims
logical :: attr_exists

call H5Aexists_by_name_f(self%file_id, dset_name, attr_name, attr_exists, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:attr_shape_check:H5Aexists_by_name_f failed: " // dset_name // ":" // attr_name
if(.not.attr_exists) error stop 'ERROR:h5fortran:attr_shape_check: attribute not exist: ' // dset_name // ":" // attr_name

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_attribute_ndims_f(self%file_id, dset_name, attr_name, attr_rank, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:attr_shape_check:get_attribute_ndims: ' // dset_name // ' ' // self%filename

if (attr_rank /= size(dims)) then
  write(stderr,'(A,I0,A,I0)') 'ERROR:h5fortran:attr_shape_check: attribute rank ' // dset_name // ':' // attr_name // ' ', &
    attr_rank,' /= ',size(dims)
  error stop
endif

!> check for matching size, else bad reads can occur.

call h5ltget_attribute_info_f(self%file_id, dset_name, attr_name, attr_dims, attr_type, attr_bytes, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:attr_shape_check:get_attribute_info' // dset_name // ':' // attr_name

if(.not. all(dims == attr_dims)) then
  write(stderr,*) 'ERROR:h5fortran:attr_shape_check: ' // dset_name // ':' // attr_name //': ', attr_dims,' /= ', dims
  error stop
endif

end subroutine attr_shape_check


subroutine get_attr_class(self, dset_name, attr_name, class, attr_id, size_bytes, pad_type)
!! get the attribute class (integer, float, string, ...)
!! {H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F}
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dset_name, attr_name
integer, intent(out) :: class
integer(HID_T), intent(in), optional :: attr_id
integer(SIZE_T), intent(out), optional :: size_bytes
integer, intent(out), optional :: pad_type

logical :: attr_exists
integer :: ierr
integer(HID_T) :: type_id, native_type_id, a_id

if(present(attr_id)) then
  a_id = attr_id
else
  call H5Aexists_by_name_f(self%file_id, dset_name, attr_name, attr_exists, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:get_attr_class:H5Aexists_by_name_f failed: " // dset_name // ":" // attr_name
  if(.not.attr_exists) error stop 'ERROR:h5fortran:get_attr_class: attribute not exist: ' // dset_name // ":" // attr_name

  call H5Aopen_by_name_f(self%file_id, dset_name, attr_name, a_id, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name_f failed: " // dset_name // ":" // attr_name
endif

call H5Aget_type_f(a_id, type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: type_id ' // dset_name // ":" // attr_name

call H5Tget_native_type_f(type_id, H5T_DIR_ASCEND_F, native_type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: native_type_id ' // dset_name // ":" // attr_name

!> compose datatype inferred
call H5Tget_class_f(native_type_id, class, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: class ' // dset_name // ":" // attr_name

if(present(size_bytes)) then
  call H5Tget_size_f(native_type_id, size_bytes, ierr)
  if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: byte size ' // dset_name // ":" // attr_name
endif

if(present(pad_type)) then
  if(class /= H5T_STRING_F) error stop "ERROR:h5fortran:get_attr_class: pad_type only for string"

  call H5Tget_strpad_f(type_id, pad_type, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:get_attr_class:h5tget_strpad " // dset_name // ":" // attr_name
endif

!> close to avoid memory leaks
call H5Tclose_f(native_type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: closing native dtype ' // dset_name // ":" // attr_name

call H5Tclose_f(type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: closing dtype ' // dset_name // ":" // attr_name

if(.not.present(attr_id)) then
  call H5Aclose_f(a_id, ierr)
  if(ierr/=0) error stop 'ERROR:h5fortran:get_class: close attribute ' // dset_name // ':' // attr_name
endif

end subroutine get_attr_class


end submodule attr_read
