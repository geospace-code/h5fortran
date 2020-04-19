program test_exist
!! test "exist" variable
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use h5fortran, only: hdf5_file, h5write

implicit none (type, external)

type(hdf5_file) :: h
character(:), allocatable :: path
character(256) :: argv
integer :: i,l

call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif
path = trim(argv) // '/foo.h5'

call h5write(path, '/x', 42)

call h%initialize(path, i)
if(i/=0) error stop
if (.not. h%exist('/x')) error stop 'x exists'

if (h%exist('/foo')) error stop 'foo not exist'

end program
