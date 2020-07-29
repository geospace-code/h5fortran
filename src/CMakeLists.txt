
if(WIN32)
  set(is_windows .true.)
else()
  set(is_windows .false.)
endif()
configure_file(pathlib.in.f90 pathlib.f90)

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
read.f90 reader.f90 ${CMAKE_CURRENT_BINARY_DIR}/reader_lt.f90
write.f90 writer.f90 ${CMAKE_CURRENT_BINARY_DIR}/writer_lt.f90
string_utils.f90 ${CMAKE_CURRENT_BINARY_DIR}/pathlib.f90)
