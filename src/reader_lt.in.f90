submodule (h5fortran:read) reader_lt

implicit none (type, external)

contains

module procedure h5exist

type(hdf5_file) :: h

call h%initialize(filename, status='old', action='r')
h5exist = h%exist(dname)
call h%finalize()

end procedure h5exist


module procedure lt0read
type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='old', action='r')

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
  ier = 6
end select
endif

if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname)) then
  if (present(ierr)) return
  error stop
endif

end procedure lt0read


module procedure lt1read
@reader_lt_template@
end procedure lt1read


module procedure lt2read
@reader_lt_template@
end procedure lt2read


module procedure lt3read
@reader_lt_template@
end procedure lt3read


module procedure lt4read
@reader_lt_template@
end procedure lt4read


module procedure lt5read
@reader_lt_template@
end procedure lt5read


module procedure lt6read
@reader_lt_template@
end procedure lt6read


module procedure lt7read
@reader_lt_template@
end procedure lt7read

end submodule reader_lt
