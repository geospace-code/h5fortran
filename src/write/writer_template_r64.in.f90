integer(HID_T) :: did, sid, mem_sid
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)

call hdf_create(self,dname, H5T_NATIVE_DOUBLE, dims, sid, did, chunk_size, istart, iend, stride, compact)

mem_sid = H5S_ALL_F !< default

if(present(istart) .and. present(iend)) then
  if(present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, did, sid, mem_sid, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, did, sid, mem_sid, istart, iend)
  endif
endif

call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier, mem_sid, sid)
if (ier/=0) error stop 'h5fortran:ERROR: could not write ' // dname // ' to ' // self%filename

call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop
