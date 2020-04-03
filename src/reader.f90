submodule (h5fortran:read) reader
!! This submodule is for reading 0-D..7-D data

use hdf5, only : h5dread_f
use h5lt, only : h5ltread_dataset_string_f

implicit none (external)

contains


module procedure hdf_read_scalar

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid
integer :: ier

ier = 0
sid = 0

if (.not.self%exist(dname)) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ier = -1
endif

if(ier == 0) then
call h5dopen_f(self%lid, dname, did, ier)

if (ier == 0) then
select type (value)
type is (character(*))
  call hdf_wrapup(did, sid, ier)  !< FIXME: till character is treated same as other types
  block
    character(len(value)) :: buf
    call h5ltread_dataset_string_f(self%lid, dname, buf, ier)
    value = buf
  end block
  return
type is (real(real64))
  call h5dread_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call h5dread_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call h5dread_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)
endif

if (present(ierr)) ierr = ier
if (ier /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  if (present(ierr)) return
  error stop
endif

end procedure hdf_read_scalar


module procedure hdf_read_1d

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid, mem_sid
integer :: ier

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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (ier /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  if (present(ierr)) return
  error stop
endif

end procedure hdf_read_1d


module procedure hdf_read_2d

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid, mem_sid
integer :: ier

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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (ier /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  if (present(ierr)) return
  error stop
endif

end procedure hdf_read_2d


module procedure hdf_read_3d

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid, mem_sid
integer :: ier

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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (ier /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  if (present(ierr)) return
  error stop
endif

end procedure hdf_read_3d


module procedure hdf_read_4d

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid, mem_sid
integer :: ier

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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (ier /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  if (present(ierr)) return
  error stop
endif

end procedure hdf_read_4d


module procedure hdf_read_5d

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid, mem_sid
integer :: ier

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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (ier /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  if (present(ierr)) return
  error stop
endif

end procedure hdf_read_5d


module procedure hdf_read_6d

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid, mem_sid
integer :: ier

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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (ier /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  if (present(ierr)) return
  error stop
endif

end procedure hdf_read_6d


module procedure hdf_read_7d

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: did, sid, mem_sid
integer :: ier

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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (ier /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  if (present(ierr)) return
  error stop
endif

end procedure hdf_read_7d


end submodule reader
