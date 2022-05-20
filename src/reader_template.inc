integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: dset_id, file_space_id, mem_space_id, xfer_id
integer :: dclass, ier

dset_id = 0 !< sentinel
file_space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
xfer_id = H5P_DEFAULT_F
dims = shape(value, HSIZE_T)

if(present(istart) .and. present(iend)) then
  call hdf_get_slice(self, dname, dset_id, file_space_id, mem_space_id, istart, iend, stride)
else
  call hdf_shape_check(self, dname, dims)
  call h5dopen_f(self%lid, dname, dset_id, ier)
  if(ier/=0) error stop 'h5fortran:reader: setup read ' // dname // ' from ' // self%filename
endif

call get_dset_class(self, dname, dclass, dset_id)

!> casting is handled by HDF5 library internally
!! select case doesn't allow H5T_*
if(dclass == H5T_FLOAT_F) then
  select type(value)
  type is (real(real64))
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (real(real32))
    call h5dread_f(dset_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, file_space_id, xfer_id)
  class default
    error stop 'h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(dclass == H5T_INTEGER_F) then
  select type(value)
  type is (integer(int32))
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (integer(int64))
    call h5dread_f(dset_id, H5T_STD_I64LE, value, dims, ier, mem_space_id, file_space_id, xfer_id)
  class default
    error stop 'h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader: closing dataset: " // dname // " in " // self%filename

if(mem_space_id /= H5S_ALL_F) call h5sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader closing memory dataspace: " // dname // " in " // self%filename

if(file_space_id /= H5S_ALL_F) call h5sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader closing file dataspace: " // dname // " in " // self%filename
