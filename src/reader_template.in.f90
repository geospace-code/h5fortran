integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: did, sid, mem_sid
integer :: ier

did = 0 !< sentinel
sid = H5S_ALL_F
mem_sid = H5S_ALL_F
dims = shape(value)

if(present(istart) .and. present(iend)) then
  if(present(stride)) then
    call hdf_get_slice(self, dname, did, sid, mem_sid, ier, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, did, sid, mem_sid, ier, istart, iend)
  endif
else
  call hdf_shape_check(self, dname, dims, ier)
  if (ier == 0) call h5dopen_f(self%lid, dname, did, ier)
endif

if(ier == 0) then
select type (value)
type is (real(real64))
  call h5dread_f(did, H5T_NATIVE_DOUBLE, value, dims, ier, mem_sid, sid)
type is (real(real32))
  call h5dread_f(did, H5T_NATIVE_REAL, value, dims, ier, mem_sid, sid)
type is (integer(int32))
  call h5dread_f(did, H5T_NATIVE_INTEGER, value, dims, ier, mem_sid, sid)
class default
  ier = 6
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop
