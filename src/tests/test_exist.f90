program test_exists
!! test "exist" variable
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use h5fortran, only: hdf5_file, h5write, h5exist, is_hdf5, hdf5_close

implicit none (type, external)

character(1024) :: argv
integer :: i,l


call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif

call test_is_hdf5(argv)
print *, 'OK: is_hdf5'

call test_exist(argv)
print *, 'OK: exist'

call test_scratch(argv)
print *, 'OK: scratch'

call test_multifiles()
print *, 'OK: multiple files open at once'

contains

subroutine test_is_hdf5(path)

character(*), intent(in) :: path
character(:), allocatable :: fn

if(is_hdf5(trim(path) // '/apidfjpj-8j9ejfpq984jfp89q39SHf.h5')) error stop 'test_exist: non-existant file declared hdf5'
fn = trim(path) // '/not_hdf5.h5'
open(newunit=i, file=fn, action='write', status='replace')
write(i,*) 'I am not an HDF5 file.'
close(i)

if(is_hdf5(fn)) error stop 'text files are not hdf5'

end subroutine test_is_hdf5


subroutine test_exist(path)

character(*), intent(in) :: path
type(hdf5_file) :: h
character(:), allocatable :: fn

fn = trim(path) // '/foo.h5'

call h5write(fn, '/x', 42)
if(.not.is_hdf5(fn)) error stop 'file does not exist'

call h%initialize(fn, i)
if(i/=0) error stop
if (.not.h%is_open) error stop 'file is open'
if (.not. h%exist('/x')) error stop 'x exists'

if (h%exist('/foo')) then
  write(stderr,*) 'variable /foo not exist in ', h%filename
  error stop
endif

call h%finalize()

if(h%is_open) error stop 'file is closed'

if (.not. h5exist(fn, '/x')) error stop 'x exists'
if (h5exist(fn, '/foo')) error stop 'foo not exist'

end subroutine test_exist


subroutine test_scratch(path)
character(*), intent(in) :: path
logical :: e
type(hdf5_file) :: h

call h%initialize(trim(path)//"/scratch.h5", status='scratch')
call h%write("/foo", 42)
call h%finalize()

inquire(file=h%filename, exist=e)
if(e) error stop 'scratch file not autodeleted'

end subroutine test_scratch


subroutine test_multifiles()

type(hdf5_file) :: f,g,h
integer :: ierr

call f%initialize(filename='A.h5', status='scratch')
call g%initialize(filename='B.h5', status='scratch')
if (h%is_open) error stop 'is_open not isolated at constructor'
call h%initialize(filename='C.h5', status='scratch')

call f%flush()

call f%finalize(ierr)
if (ierr/=0) error stop 'close a.h5'
if (.not.g%is_open .or. .not. h%is_open) error stop 'is_open not isolated at destructor'
call g%finalize(ierr)
if (ierr/=0) error stop 'close b.h5'
call h%finalize(ierr)
if (ierr/=0) error stop 'close c.h5'

call hdf5_close()

end subroutine test_multifiles

end program
