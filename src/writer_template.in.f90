integer(HID_T) :: did, sid, mem_sid
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

sid = H5S_ALL_F
mem_sid = H5S_ALL_F

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims, sid, did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  ier = 6
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop
