integer(HID_T) :: file_space_id, mem_space_id, dset_id, xfer_id, dtype, dtype_id
integer(HSIZE_T), dimension(rank(A)) :: mem_dims, dims_dset
integer :: ier, charlen

xfer_id = H5P_DEFAULT_F

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
  charlen = len(A)
  dtype = H5T_NATIVE_CHARACTER
class default
  error stop "unknown variable type for " // dname
end select

call hdf_create(self, dname, dtype, mem_dims, dims_dset, file_space_id, dset_id, &
  chunk_size=chunk_size, istart=istart, iend=iend, stride=stride, compact=compact, &
  dtype_id=dtype_id, charlen=charlen)

mem_space_id = H5S_ALL_F !< default

if(present(istart) .and. present(iend)) then
  call hdf_get_slice(self, dname, dset_id, file_space_id, mem_space_id, istart, iend, stride)
endif

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
  call h5dwrite_f(dset_id, dtype_id, A, dims_dset, ier)
  if (ier /= 0) error stop 'h5fortran:write:string: could not write ' // dname // ' to ' // self%filename
  call h5tclose_f(dtype_id, ier)
class default
  error stop "ERROR:h5fortran:writer: unknown variable type for " // dname
end select
if (ier /= 0) error stop 'ERROR:h5fortran:h5dwrite: could not write ' // dname // ' to ' // self%filename

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer: closing dataset: " // dname // " in " // self%filename

if(mem_space_id /= H5S_ALL_F) call h5sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing memory dataspace: " // dname // " in " // self%filename

if(file_space_id /= H5S_ALL_F) call h5sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing file dataspace: " // dname // " in " // self%filename
