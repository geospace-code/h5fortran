submodule (h5fortran:write) write_scalar

use hdf5, only: H5Dwrite_f

implicit none

contains


module procedure h5write_scalar

integer(HSIZE_T) :: dset_dims(0), mem_dims(0)
integer(HID_T) :: file_space_id, dset_id, dtype_id, xfer_id, dtype
integer :: ier, charlen

charlen = 0

select type (A)
type is (real(real32))
  dtype = H5T_NATIVE_REAL
type is (real(real64))
  dtype = H5T_NATIVE_DOUBLE
type is (integer(int32))
  dtype = H5T_NATIVE_INTEGER
type is (integer(int64))
  dtype = H5T_STD_I64LE
type is (character(*))
  dtype = H5T_NATIVE_CHARACTER
  charlen = len_trim(A)  !< workaround for GCC 8.3.0 bug
  if(charlen == 0) charlen = 1 !< empty string is OK but charlen is strictly positive.
class default
  error stop "ERROR:h5fortran:write: unknown variable type for " // dname
end select

call hdf_create(self, dname, dtype, mem_dims=mem_dims, dset_dims=dset_dims, &
  filespace_id=file_space_id, dset_id=dset_id, compact=compact, dtype_id=dtype_id, &
  charlen=charlen)

xfer_id = H5P_DEFAULT_F

select type (A)
type is (real(real32))
  call H5Dwrite_f(dset_id, dtype, A, dset_dims, ier, file_space_id=file_space_id, xfer_prp=xfer_id)
type is (real(real64))
  call H5Dwrite_f(dset_id, dtype, A, dset_dims, ier, file_space_id=file_space_id, xfer_prp=xfer_id)
type is (integer(int32))
  call H5Dwrite_f(dset_id, dtype, A, dset_dims, ier, file_space_id=file_space_id, xfer_prp=xfer_id)
type is (integer(int64))
  call H5Dwrite_f(dset_id, dtype, A, dset_dims, ier, file_space_id=file_space_id, xfer_prp=xfer_id)
type is (character(*))
  call H5Dwrite_f(dset_id, dtype_id, trim(A), dset_dims, ier, file_space_id=file_space_id, xfer_prp=xfer_id)
class default
  error stop "ERROR:h5fortran:write: unsupported type for " // dname
end select
call estop(ier, "write_scalar:H5Dwrite", self%filename, dname)

call H5Tclose_f(dtype_id, ier)
call estop(ier, "write_scalar:H5Tclose", self%filename, dname)

call H5Dclose_f(dset_id, ier)
call estop(ier, "write_scalar:H5Dclose", self%filename, dname)

call H5Sclose_f(file_space_id, ier)
call estop(ier, "write_scalar:H5Sclose", self%filename, dname)


end procedure h5write_scalar

end submodule write_scalar
