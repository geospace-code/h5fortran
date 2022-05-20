integer(HID_T) :: file_space_id, mem_space_id, dset_id, xfer_id, dtype
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

xfer_id = H5P_DEFAULT_F

dims = shape(value, HSIZE_T)

select type (value)
type is (real(real32))
  dtype = H5T_NATIVE_REAL
type is (real(real64))
  dtype = H5T_NATIVE_DOUBLE
type is (integer(int32))
  dtype = H5T_NATIVE_INTEGER
type is (integer(int64))
  dtype = H5T_STD_I64LE
class default
  error stop "unknown variable type for " // dname
end select

call hdf_create(self, dname, dtype, dims, file_space_id, dset_id, &
  chunk_size=chunk_size, istart=istart, iend=iend, stride=stride, compact=compact)

mem_space_id = H5S_ALL_F !< default

if(present(istart) .and. present(iend)) then
  call hdf_get_slice(self, dname, dset_id, file_space_id, mem_space_id, istart, iend, stride)
endif

select type (value)
type is (real(real32))
  call h5dwrite_f(dset_id, dtype, value, dims, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (real(real64))
  call h5dwrite_f(dset_id, dtype, value, dims, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (integer(int32))
  call h5dwrite_f(dset_id, dtype, value, dims, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (integer(int64))
  call h5dwrite_f(dset_id, dtype, value, dims, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
class default
  error stop "unknown variable type for " // dname
end select
if (ier/=0) error stop 'h5fortran:ERROR: could not write ' // dname // ' to ' // self%filename

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer: closing dataset: " // dname // " in " // self%filename

if(mem_space_id /= H5S_ALL_F) call h5sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing memory dataspace: " // dname // " in " // self%filename

if(file_space_id /= H5S_ALL_F) call h5sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing file dataspace: " // dname // " in " // self%filename
