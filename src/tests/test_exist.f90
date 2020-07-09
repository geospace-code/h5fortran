program test_exist
!! test "exist" variable
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use h5fortran, only: hdf5_file, h5write, h5exist, is_hdf5

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

! --- test file is / is not hdf5
if(is_hdf5(trim(argv) // '/apidfjpj-8j9ejfpq984jfp89q39SHf.h5')) error stop 'test_exist: non-existant file declared hdf5'
path = trim(argv) // 'not_hdf5.h5'
open(newunit=i, file=path, action='write', status='replace')
write(i,*) 'I am not an HDF5 file.'
close(i)

if(is_hdf5(path)) error stop 'text files are not hdf5'

! --- test variable exists
path = trim(argv) // '/foo.h5'

call h5write(path, '/x', 42)
if(.not.is_hdf5(path)) error stop 'hdf5 file does not exist'

call h%initialize(path, i)
if(i/=0) error stop
if (.not. h%exist('/x')) error stop 'x exists'

if (h%exist('/foo')) error stop 'foo not exist'

call h%finalize()

if (.not. h5exist(path, '/x')) error stop 'x exists'
if (h5exist(path, '/foo')) error stop 'foo not exist'

end program
