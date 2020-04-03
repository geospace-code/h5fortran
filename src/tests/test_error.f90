program test_error

use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit
use h5fortran, only: hdf5_file

implicit none (external)

type(hdf5_file) :: h5f

character(:), allocatable :: path
character(256) :: argv
integer :: i,l

call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif
path = trim(argv)
print *, 'test path: ', path

call test_nonexist_old_file(path)
print *, 'OK: non-existing old file'
call test_nonexist_unknown_file(path)
print *, 'OK: non-existing unknown file'
call test_nonhdf5_file(path)
print *, 'OK: non-HDF5 file'
call test_nonexist_variable(path)
print *, 'OK: non-existing variable'
call test_wrong_type(path)
print *, "OK: wrong type read"
call test_unknown_write(path)
print *, 'OK: unknown write'
call test_unknown_read(path)
print *, 'OK: unknown read'

contains


subroutine test_nonexist_old_file(path)
character(*), intent(in) :: path
integer :: ierr

call h5f%initialize(path//'/not-exist', ierr, status='old', action='read', verbose=.false.)
if (ierr==0) error stop 'should have had ierr/=0 on non-existing old file'
end subroutine test_nonexist_old_file


subroutine test_nonexist_unknown_file(path)
character(*), intent(in) :: path
integer :: ierr

call h5f%initialize(path//'/not-exist', ierr, status='unknown', action='read', verbose=.false.)
if (ierr==0) error stop 'should have had ierr/=0 on non-existing unknown read file'
end subroutine test_nonexist_unknown_file


subroutine test_nonhdf5_file(path)
character(*), intent(in) :: path
integer :: u,ierr
character(:), allocatable :: filename

filename = path // '/junk.dat'

! create or replace zero-length file, could be any size, just not a valid HDF5 file
open(newunit=u, file=filename, status='replace', iostat=ierr, action='write')
close(u)

call h5f%initialize(filename, ierr, status='old', action='read')
if (ierr==0) error stop 'should have had ierr/=0 on invalid HDF5 file'
end subroutine test_nonhdf5_file


subroutine test_nonexist_variable(path)
character(*), intent(in) :: path
integer :: u,ierr
character(:), allocatable :: filename

filename = path // '/junk.h5'
call h5f%initialize(filename, status='replace', action='readwrite', verbose=.false.)
call h5f%read('/not-exist', u, ierr)
if(ierr==0) error stop 'test_nonexist_variable: should have ierr/=0 on non-exist variable'
call h5f%finalize()
end subroutine test_nonexist_variable


subroutine test_wrong_type(path)
character(*), intent(in) :: path
integer :: u
character(:), allocatable :: filename

print *, 'test_wrong_type: write'

filename = path // '/junk.h5'
call h5f%initialize(filename, status='replace', action='write', verbose=.false.)
call h5f%write('/real32', 42.)
call h5f%finalize()


print *, 'test_wrong_type: read'

call h5f%initialize(filename, status='old', action='read', verbose=.false.)
call h5f%read('/real32', u)
if (u /= 42) error stop 'test_wrong_type: did not coerce real to integer'
call h5f%finalize()

end subroutine test_wrong_type


subroutine test_unknown_write(path)
character(*), intent(in) :: path
integer :: ierr
character(:), allocatable :: filename
complex :: x

x = (1, -1)

filename = path // '/junk.h5'
call h5f%initialize(filename, ierr, status='replace', action='write', verbose=.false.)
if(ierr/=0) error stop 'test_unknown_write: creating file'
call h5f%write('/complex', x, ierr)
if(ierr==0) error stop 'test_unknown_write: writing unknown type variable'
end subroutine test_unknown_write


subroutine test_unknown_read(path)
character(*), intent(in) :: path
integer :: ierr
character(:), allocatable :: filename
complex :: x

x = (1, -1)

filename = path // '/junk.h5'
call h5f%initialize(filename, status='unknown', action='readwrite', verbose=.false.)
call h5f%read('/complex', x, ierr)
if(ierr==0) error stop 'test_unknown_read: reading unknown type variable'
end subroutine test_unknown_read

end program
