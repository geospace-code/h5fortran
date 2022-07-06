submodule (h5fortran:attr_smod) attr_write

use hdf5, only: H5Awrite_f

implicit none (type, external)

contains

module procedure writeattr_scalar

integer(HID_T) :: attr_id, dtype_id, space_id
integer(HSIZE_T) :: attr_dims(0)
integer :: ier, L

select type (A)
type is (real(real32))
  call attr_create(self, obj_name, attr, H5T_NATIVE_REAL, attr_dims, space_id, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_REAL, A, attr_dims, ier)
type is (real(real64))
  call attr_create(self, obj_name, attr, H5T_NATIVE_DOUBLE, attr_dims, space_id, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_DOUBLE, A, attr_dims, ier)
type is (integer(int32))
  call attr_create(self, obj_name, attr, H5T_NATIVE_INTEGER, attr_dims, space_id, attr_id)
  call H5Awrite_f(attr_id, H5T_NATIVE_INTEGER, A, attr_dims, ier)
type is (integer(int64))
  call attr_create(self, obj_name, attr, H5T_STD_I64LE, attr_dims, space_id, attr_id)
  call H5Awrite_f(attr_id, H5T_STD_I64LE, A, attr_dims, ier)
type is (character(*))
  L = len(A)  !< workaround for GCC 8.3.0 bug
  call attr_create(self, obj_name, attr, H5T_NATIVE_CHARACTER, attr_dims, space_id, attr_id, dtype_id, charlen=L)
  call H5Awrite_f(attr_id, dtype_id, A, attr_dims, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:writeattr:string:H5Awrite: ' // obj_name // ":" // attr // " in " // self%filename
  call h5tclose_f(dtype_id, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:writeattr:string:H5Tclose ' // obj_name // ":" // attr // " in " // self%filename
class default
  error stop "ERROR:h5fortran:writeattr: unsupported type for " // obj_name // ":" // attr // " in " // self%filename
end select
if (ier /= 0) error stop 'ERROR:h5fortran:writeattr:H5Awrite ' // obj_name // ":" // attr // " in " // self%filename

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writeattr:H5Aclose " // obj_name // ":" // attr // " in " // self%filename

if(space_id /= H5S_ALL_F) call H5Sclose_f(space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writeattr:H5Sclose " // obj_name // ":" // attr // " in " // self%filename

end procedure writeattr_scalar


module procedure writeattr_1d
include 'attr_write.inc'
end procedure

module procedure writeattr_2d
include 'attr_write.inc'
end procedure

module procedure writeattr_3d
include 'attr_write.inc'
end procedure

module procedure writeattr_4d
include 'attr_write.inc'
end procedure

module procedure writeattr_5d
include 'attr_write.inc'
end procedure

module procedure writeattr_6d
include 'attr_write.inc'
end procedure

module procedure writeattr_7d
include 'attr_write.inc'
end procedure


module procedure lt0writeattr

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr(obj_name, attr, A)
call h%close()

end procedure lt0writeattr


module procedure lt1writeattr

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr(obj_name, attr, A)
call h%close()

end procedure lt1writeattr


end submodule attr_write
