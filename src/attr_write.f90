submodule (h5fortran) attr_write

use hdf5, only : H5Aopen_by_name_f, H5Awrite_f, H5Aclose_f, H5Aexists_by_name_f, H5Acreate_by_name_f, &
H5S_SCALAR_F, &
H5Screate_f, H5Screate_simple_f, H5Sclose_f, &
H5Tcopy_f, H5Tset_size_f, H5Tclose_f
use h5lt, only: h5ltset_attribute_string_f, h5ltset_attribute_float_f, h5ltset_attribute_double_f, h5ltset_attribute_int_f

implicit none (type, external)

contains

module procedure writeattr_scalar

integer(HID_T) :: attr_id, dtype_id
integer(HSIZE_T) :: attr_dims(0)
integer :: ier

select type (A)
type is (real(real32))
  call attr_create(self, dname, attr, H5T_NATIVE_REAL, attr_dims, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_REAL, A, attr_dims, ier)
type is (real(real64))
  call attr_create(self, dname, attr, H5T_NATIVE_DOUBLE, attr_dims, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_DOUBLE, A, attr_dims, ier)
type is (integer(int32))
  call attr_create(self, dname, attr, H5T_NATIVE_INTEGER, attr_dims, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_INTEGER, A, attr_dims, ier)
type is (integer(int64))
  call attr_create(self, dname, attr, H5T_STD_I64LE, attr_dims, attr_id)
  call H5Awrite_f(attr_id, H5T_STD_I64LE, A, attr_dims, ier)
type is (character(*))
  call attr_create(self, dname, attr, H5T_NATIVE_CHARACTER, attr_dims, attr_id, dtype_id, charlen=len(A))
  call H5Awrite_f(attr_id, dtype_id, A, attr_dims, ier)
  if (ier /= 0) error stop 'h5fortran:writeattr:string: could not write ' // dname // ":" // attr // " in " // self%filename
  call h5tclose_f(dtype_id, ier)
class default
  error stop "ERROR:h5fortran:writeattr: unsupported type for " // dname // ":" // attr // " in " // self%filename
end select
if (ier /= 0) error stop 'ERROR:h5fortran:writeattr: could not write ' // dname // ":" // attr // " in " // self%filename

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writeattr: closing " // dname // ":" // attr // " in " // self%filename

end procedure writeattr_scalar


module procedure writeattr_1d

integer :: ier
integer(size_t) :: dsize

if(.not.self%is_open()) error stop 'ERROR:h5fortran:writeattr: file handle is not open'

dsize = size(A)

select type(A)
type is (real(real32))
  call h5ltset_attribute_float_f(self%file_id, dname, attr, A, dsize, ier)
type is (real(real64))
  call h5ltset_attribute_double_f(self%file_id, dname, attr, A, dsize, ier)
type is (integer(int32))
  call h5ltset_attribute_int_f(self%file_id, dname, attr, A, dsize, ier)
class default
  error stop "ERROR:h5fortran:writeattr_num: unknown dataset type for " // dname // ":" // attr // " in " // self%filename
end select

if (ier /= 0) error stop "ERROR:h5fortran:writeattr_num: " // dname // ":" // attr // " in " // self%filename

end procedure writeattr_1d


module procedure lt0writeattr

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr(dname, attr, A)
call h%close()

end procedure lt0writeattr


module procedure lt1writeattr

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr(dname, attr, A)
call h%close()

end procedure lt1writeattr


subroutine attr_create(self, dname, attr, dtype, attr_dims, attr_id, dtype_id, charlen)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: attr_dims
integer(HID_T), intent(out) :: attr_id
integer(HID_T), intent(out), optional :: dtype_id
integer, intent(in), optional :: charlen !< length of character scalar

logical :: attr_exists
integer :: ier
integer(HID_T) :: filespace_id, type_id


if(dtype == H5T_NATIVE_CHARACTER) then
  if(.not. present(dtype_id)) error stop "ERROR:h5fortran:attr_create: character needs type_id"
  if(.not. present(charlen)) error stop "ERROR:h5fortran:attr_create: character type must specify charlen"
endif

if(.not.self%is_open()) error stop 'h5fortran:writeattr: file handle is not open'

call H5Aexists_by_name_f(self%file_id, dname, attr, attr_exists, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attrwrite:H5Aexists_by_name failed: " // dname // ":" // attr // " in " // self%filename

if(attr_exists) then
  call H5Aopen_by_name_f(self%file_id, dname, attr, attr_id, ier)
  if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name failed: " // dname // ":" // attr // " in " // self%filename

  if(dtype == H5T_NATIVE_CHARACTER) then
    call h5tcopy_f(dtype, type_id, ier)
    if(ier /= 0) error stop "h5fortran:h5tcopy:character: " // dname // ' in ' // self%filename

    call h5tset_size_f(type_id, int(charlen, SIZE_T), ier)
    if(ier /= 0) error stop "h5fortran:h5tset_size:character: " // dname // ' in ' // self%filename

    dtype_id = type_id
  endif

  return
endif

!> create attribute dataspace
if(size(attr_dims) == 0) then
  call H5Screate_f(H5S_SCALAR_F, filespace_id, ier)
else
  call H5Screate_simple_f(size(attr_dims), attr_dims, filespace_id, ier)
endif
if (ier /= 0) error stop "ERROR:h5fortran:hdf_create:h5screate:filespace " // dname // " in " // self%filename

if(dtype == H5T_NATIVE_CHARACTER) then
  call h5tcopy_f(dtype, type_id, ier)
  if(ier /= 0) error stop "h5fortran:h5tcopy:character: " // dname // ' in ' // self%filename

  call h5tset_size_f(type_id, int(charlen, SIZE_T), ier)
  if(ier /= 0) error stop "h5fortran:h5tset_size:character: " // dname // ' in ' // self%filename
  dtype_id = type_id
else
  type_id = dtype
endif

call H5Acreate_by_name_f(self%file_id, dname, attr, type_id, filespace_id, attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Acreate_by_name failed: " // dname // ":" // attr // " in " // self%filename

call H5Sclose_f(filespace_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attr_create:h5sclose: space close " // dname // ":" // attr // " in " // self%filename


end subroutine attr_create

end submodule attr_write
