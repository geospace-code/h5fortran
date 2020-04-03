submodule (h5fortran:read) reader_lt

implicit none (external)

contains

module procedure lt0read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')

if (ier == 0) then
select type (value)
type is (character(*))
  call h%read(dname, value, ier)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt0read


module procedure lt1read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt1read


module procedure lt2read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt2read


module procedure lt3read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt3read


module procedure lt4read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt4read


module procedure lt5read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt5read


module procedure lt6read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt6read


module procedure lt7read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old')

if (ier == 0) then
select type (value)
type is (real(real64))
  call h%read(dname, value, ier)
type is (real(real32))
  call h%read(dname, value, ier)
type is (integer(int32))
  call h%read(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled by h5fortran.'
  ier = -1
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' read_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt7read

end submodule reader_lt