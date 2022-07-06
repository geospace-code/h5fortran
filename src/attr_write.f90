submodule (h5fortran:attr_smod) attr_write

use h5lt, only: h5ltset_attribute_string_f, h5ltset_attribute_float_f, h5ltset_attribute_double_f, h5ltset_attribute_int_f

implicit none (type, external)

contains

module procedure writeattr_scalar

integer(HID_T) :: attr_id, dtype_id
integer(HSIZE_T) :: attr_dims(0)
integer :: ier, L

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
  L = len(A)  !< workaround for GCC 8.3.0 bug
  call attr_create(self, dname, attr, H5T_NATIVE_CHARACTER, attr_dims, attr_id, dtype_id, charlen=L)
  call H5Awrite_f(attr_id, dtype_id, A, attr_dims, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:writeattr:string:H5Awrite: ' // dname // ":" // attr // " in " // self%filename
  call h5tclose_f(dtype_id, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:writeattr:string:H5Tclose ' // dname // ":" // attr // " in " // self%filename
class default
  error stop "ERROR:h5fortran:writeattr: unsupported type for " // dname // ":" // attr // " in " // self%filename
end select
if (ier /= 0) error stop 'ERROR:h5fortran:writeattr:H5Awrite ' // dname // ":" // attr // " in " // self%filename

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


end submodule attr_write
