submodule (h5fortran:write) write_scalar

use hdf5, only: h5dwrite_f

implicit none (type, external)

contains

module procedure h5write_scalar

integer(HID_T)  :: file_space_id, dset_id, dtype_id
integer(HSIZE_T) :: dims(0)
integer :: ier

if(.not.self%is_open()) error stop 'h5fortran:write: file handle is not open'

select type (value)
type is (real(real32))
  call hdf_create(self, dname, H5T_NATIVE_REAL, dims, file_space_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_REAL, value, dims, ier)
type is (real(real64))
  call hdf_create(self, dname, H5T_NATIVE_DOUBLE, dims, file_space_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (integer(int32))
  call hdf_create(self, dname, H5T_NATIVE_INTEGER, dims, file_space_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, value, dims, ier)
type is (integer(int64))
  call hdf_create(self, dname, H5T_STD_I64LE, dims, file_space_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_STD_I64LE, value, dims, ier)
type is (character(*))
  call hdf_create(self, dname, H5T_NATIVE_CHARACTER, dims, file_space_id, dset_id, dtype_id, charlen=len(value))
  call h5dwrite_f(dset_id, dtype_id, value, dims, ier)
  if (ier /= 0) error stop 'h5fortran:write:string: could not write ' // dname // ' to ' // self%filename
  call h5tclose_f(dtype_id, ier)
class default
  error stop "ERROR:h5fortran:write: unsupported type for " // dname
end select
if (ier /= 0) error stop 'ERROR:h5fortran:write: could not write ' // dname // ' to ' // self%filename

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer: closing dataset: " // dname // " in " // self%filename

if(file_space_id /= H5S_ALL_F) call h5sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing file dataspace: " // dname // " in " // self%filename


end procedure h5write_scalar

end submodule write_scalar
