integer(HID_T) :: did, sid, mem_sid
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims, sid, did, ier, chunk_size)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims, sid, did, ier, chunk_size)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims, sid, did, ier, chunk_size)
class default
  ier = 6
end select

mem_sid = H5S_ALL_F !< default

if (ier==0) then
if(present(istart) .and. present(iend)) then
  if(present(stride)) then
    call hdf_get_slice(self, dname, did, sid, mem_sid, ier, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, did, sid, mem_sid, ier, istart, iend)
  endif
endif
endif

if (ier==0) then
select type (value)
type is (real(real64))
  call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier, mem_sid, sid)
type is (real(real32))
  call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier, mem_sid, sid)
type is (integer(int32))
  call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier, mem_sid, sid)
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop
