submodule (h5fortran:write) writer
!! This submodule is for writing 0-D..7-D data

implicit none

contains


module procedure hdf_write_scalar

integer(HID_T)  :: sid, did
integer(HSIZE_T), allocatable :: dims(:)
integer :: ier

allocate(dims(0))

select type (value)
type is (character(*))
  call h5ltmake_dataset_string_f(self%lid, dname, value, ier)
  if (present(ierr)) ierr = ier
  if (ier /= 0) then
    write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
    if (present(ierr)) return
    error stop
  endif
  return
type is (real(real64))
  !! NOTE: 0d does not use chunk_size
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_scalar


module procedure hdf_write_1d

integer(HID_T)  :: sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  !! NOTE: 1d does not use chunk_size
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_1d


module procedure hdf_write_2d

integer(HID_T)  :: sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_2d


module procedure hdf_write_3d

integer(HID_T)  :: sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_3d


module procedure hdf_write_4d

integer(HID_T)  :: sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_4d


module procedure hdf_write_5d

integer(HID_T)  :: sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_5d


module procedure hdf_write_6d

integer(HID_T)  :: sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_6d


module procedure hdf_write_7d

integer(HID_T)  :: sid, did
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value)
select type (value)
type is (real(real64))
  call hdf_setup_write(self,dname, H5T_NATIVE_DOUBLE, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_DOUBLE, value, dims, ier)
type is (real(real32))
  call hdf_setup_write(self,dname, H5T_NATIVE_REAL, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_REAL, value, dims, ier)
type is (integer(int32))
  call hdf_setup_write(self,dname, H5T_NATIVE_INTEGER, dims,sid,did, ier, chunk_size)
  if (ier == 0) call h5dwrite_f(did, H5T_NATIVE_INTEGER, value, dims, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_write_7d


end submodule writer
