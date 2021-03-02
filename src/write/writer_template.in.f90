integer(HID_T) :: did, sid, mem_sid
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_create(self,dname, H5T_NATIVE_DOUBLE, dims, sid, did, chunk_size, istart, iend, stride, compact)
type is (real(real32))
  call hdf_create(self,dname, H5T_NATIVE_REAL, dims, sid, did, chunk_size, istart, iend, stride, compact)
type is (integer(int32))
  call hdf_create(self,dname, H5T_NATIVE_INTEGER, dims, sid, did, chunk_size, istart, iend, stride, compact)
type is (integer(int64))
  call hdf_create(self,dname, H5T_STD_I64LE, dims, sid, did, chunk_size, istart, iend, stride, compact)
class default
  error stop 'h5fortran:write:invalid data type'
end select

mem_sid = H5S_ALL_F !< default

if(present(istart) .and. present(iend)) then
  if(present(stride)) then
    !! necessary to use this present check for Intel and GCC
    call hdf_get_slice(self, dname, did, sid, mem_sid, istart, iend, stride)
  else
    call hdf_get_slice(self, dname, did, sid, mem_sid, istart, iend)
  endif
endif

select type (value)
type is (real(real64))
  call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier, mem_sid, sid)
type is (real(real32))
  call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier, mem_sid, sid)
type is (integer(int32))
  call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier, mem_sid, sid)
type is (integer(int64))
  call h5dwrite_f(did, H5T_STD_I64LE, value, dims, ier, mem_sid, sid)
end select
if (ier/=0) error stop 'h5fortran:ERROR: could not write ' // dname // ' to ' // self%filename

call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not.present(ierr)) error stop
