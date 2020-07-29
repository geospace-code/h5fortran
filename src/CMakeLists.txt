
if(WIN32)
  set(is_windows .true.)
else()
  set(is_windows .false.)
endif()
configure_file(pathlib.in.f90 pathlib.f90)

set(reader_template "
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
  ier = 6
end select
endif

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname)) then
  if (present(ierr)) return
  error stop
endif
")
configure_file(reader.in.f90 reader.f90)

set(reader_lt_template "
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old', action='r')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  ier = 6
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname)) then
  if (present(ierr)) return
  error stop
endif
")
configure_file(reader_lt.in.f90 reader_lt.f90)

set(writer_template "
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
  ier = 6
end select

if(ier == 0) call hdf_wrapup(did, sid, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname)) then
  if (present(ierr)) return
  error stop
endif
")
configure_file(writer.in.f90 writer.f90)

set(writer_lt_template "
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  ier = 6
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname)) then
  if (present(ierr)) return
  error stop
endif
")
configure_file(writer_lt.in.f90 writer_lt.f90)

target_sources(h5fortran PRIVATE
interface.f90
read.f90 ${CMAKE_CURRENT_BINARY_DIR}/reader.f90 ${CMAKE_CURRENT_BINARY_DIR}/reader_lt.f90
write.f90 ${CMAKE_CURRENT_BINARY_DIR}/writer.f90 ${CMAKE_CURRENT_BINARY_DIR}/writer_lt.f90
string_utils.f90 ${CMAKE_CURRENT_BINARY_DIR}/pathlib.f90)
