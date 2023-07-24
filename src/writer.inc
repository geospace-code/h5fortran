integer(HID_T) :: file_space_id, mem_space_id, dset_id, xfer_id, dtype, dtype_id
integer(HSIZE_T), dimension(rank(A)) :: mem_dims, dims_dset
integer :: ier, charlen

mem_dims = shape(A, HSIZE_T)

if(present(dset_dims)) then
  dims_dset = dset_dims
else
  dims_dset = mem_dims
endif

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
  charlen = len(A)  !< workaround for GCC 8.3.0 bug
  if(charlen == 0) charlen = 1 !< empty string is OK but charlen is strictly positive.
class default
  error stop "ERROR:h5fortran:writer:unknown variable type for " // dname
end select

call hdf_create(self, dname, dtype, mem_dims, dims_dset, file_space_id, dset_id, &
  chunk_size=chunk_size, istart=istart, iend=iend, stride=stride, compact=compact, &
  dtype_id=dtype_id, charlen=charlen)

if(present(istart) .and. present(iend)) then
  call hdf_get_slice(mem_dims, dset_id, file_space_id, mem_space_id, istart, iend, stride=stride, dset_dims=dims_dset)
else
  call H5Dget_space_f(dset_id, mem_space_id, ier)
  call estop(ier, "writer:H5Dget_space", self%filename, dname)
endif

xfer_id = H5P_DEFAULT_F

select type (A)
type is (real(real32))
  call h5dwrite_f(dset_id, dtype, A, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (real(real64))
  call h5dwrite_f(dset_id, dtype, A, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (integer(int32))
  call h5dwrite_f(dset_id, dtype, A, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (integer(int64))
  call h5dwrite_f(dset_id, dtype, A, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (character(*))
  call h5dwrite_f(dset_id, dtype_id, A, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
class default
  error stop "ERROR:h5fortran:writer: unknown variable type for " // dname
end select
call estop(ier, "writer:H5Dwrite", self%filename, dname)

call H5Tclose_f(dtype_id, ier)
call estop(ier, "writer:H5Tclose", self%filename, dname)

call H5Dclose_f(dset_id, ier)
call estop(ier, "writer:H5Dclose", self%filename, dname)

call H5Sclose_f(mem_space_id, ier)
call estop(ier, "writer:H5Sclose", self%filename, dname)

call H5Sclose_f(file_space_id, ier)
call estop(ier, "writer:H5Sclose", self%filename, dname)
