integer(HID_T) :: did, sid, mem_sid, dtype
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

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

call hdf_create(self, dname, dtype, dims, sid, did, chunk_size, istart, iend, stride, compact)

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
type is (real(real32))
  call h5dwrite_f(did, dtype, value, dims, ier, mem_sid, sid)
type is (real(real64))
  call h5dwrite_f(did, dtype, value, dims, ier, mem_sid, sid)
type is (integer(int32))
  call h5dwrite_f(did, dtype, value, dims, ier, mem_sid, sid)
type is (integer(int64))
  call h5dwrite_f(did, dtype, value, dims, ier, mem_sid, sid)
class default
  error stop "unknown variable type for " // dname
end select
if (ier/=0) error stop 'h5fortran:ERROR: could not write ' // dname // ' to ' // self%filename

call hdf_wrapup(did, sid)
