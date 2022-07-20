submodule (h5fortran:write) write_scalar

use hdf5, only: h5dwrite_f

implicit none (type, external)

contains


module procedure h5write_scalar

integer(HID_T) :: file_space_id, dset_id, dtype_id
integer(HSIZE_T) :: dims(0)
integer :: ier, L

if(.not.self%is_open()) error stop 'ERROR:h5fortran:write: file handle is not open'

select type (A)
type is (real(real32))
  call hdf_create(self, dname, H5T_NATIVE_REAL, dims, dims, file_space_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_REAL, A, dims, ier)
type is (real(real64))
  call hdf_create(self, dname, H5T_NATIVE_DOUBLE, dims, dims, file_space_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, A, dims, ier)
type is (integer(int32))
  call hdf_create(self, dname, H5T_NATIVE_INTEGER, dims, dims, file_space_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, A, dims, ier)
type is (integer(int64))
  call hdf_create(self, dname, H5T_STD_I64LE, dims, dims, file_space_id, dset_id, compact=compact)
  call h5dwrite_f(dset_id, H5T_STD_I64LE, A, dims, ier)
type is (character(*))
  L = len(A)  !< workaround for GCC 8.3.0 bug
  call hdf_create(self, dname, H5T_NATIVE_CHARACTER, dims, dims, file_space_id, dset_id, dtype_id, charlen=L)
  call h5dwrite_f(dset_id, dtype_id, A, dims, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:write:string: could not write ' // dname // ' to ' // self%filename
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
