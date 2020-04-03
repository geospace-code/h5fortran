submodule (h5fortran:write) writer_lt

implicit none (external)

contains


module procedure lt0write
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')

select type (value)
type is (character(*))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real64))
  if (ier == 0) call h%write(dname, value, ier)
type is (real(real32))
  if (ier == 0) call h%write(dname, value, ier)
type is (integer(int32))
  if (ier == 0) call h%write(dname, value, ier)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt0write


module procedure lt1write
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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt1write


module procedure lt2write
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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt2write


module procedure lt3write
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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt3write


module procedure lt4write
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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt4write


module procedure lt5write
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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt5write


module procedure lt6write
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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt6write


module procedure lt7write
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
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ier = -1
end select

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: ' // dname // ' write_lt ' // filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt7write

end submodule writer_lt