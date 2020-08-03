program test_error

use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit
use h5fortran, only: hdf5_file

implicit none (type, external)

type(hdf5_file) :: h5f


call test_nonexist_old_file()
print *, 'OK: non-existing old file'
call test_nonexist_unknown_file()
print *, 'OK: non-existing unknown file'
call test_nonhdf5_file()
print *, 'OK: non-HDF5 file'
call test_nonexist_variable()
print *, 'OK: non-existing variable'
call test_wrong_type()
print *, "OK: wrong type read"
call test_unknown_write()
print *, 'OK: unknown write'
call test_unknown_read()
print *, 'OK: unknown read'

contains


subroutine test_nonexist_old_file()
integer :: ierr

call h5f%initialize('not-exist.h5', ierr, status='old', action='read', verbose=.false.)
if (ierr==0) error stop 'should have had ierr/=0 on non-existing old file'
end subroutine test_nonexist_old_file


subroutine test_nonexist_unknown_file()
integer :: ierr

call h5f%initialize('not-exist.h5', ierr, status='unknown', action='read', verbose=.false.)
if (ierr==0) error stop 'should have had ierr/=0 on non-existing unknown read file'
end subroutine test_nonexist_unknown_file


subroutine test_nonhdf5_file()
integer :: u,ierr
character(*), parameter :: filename = 'bad.h5'

! create or replace zero-length file, could be any size, just not a valid HDF5 file
open(newunit=u, file=filename, status='replace', iostat=ierr, action='write')
close(u)

call h5f%initialize(filename, ierr, status='old', action='read')
if (ierr==0) error stop 'should have had ierr/=0 on invalid HDF5 file'
end subroutine test_nonhdf5_file


subroutine test_nonexist_variable()
integer :: u,ierr
character(*), parameter :: filename = 'bad.h5'

call h5f%initialize(filename, status='replace', action='readwrite', verbose=.false.)
call h5f%read('/not-exist', u, ierr)
if(ierr==0) error stop 'test_nonexist_variable: should have ierr/=0 on non-exist variable'
call h5f%finalize()
end subroutine test_nonexist_variable


subroutine test_wrong_type()
integer :: u
character(*), parameter :: filename = 'bad.h5'

print *, 'test_wrong_type: write'

call h5f%initialize(filename, status='replace', action='write', verbose=.false.)
call h5f%write('/real32', 42.)
call h5f%finalize()


print *, 'test_wrong_type: read'

call h5f%initialize(filename, status='old', action='read', verbose=.false.)
call h5f%read('/real32', u)
if (u /= 42) error stop 'test_wrong_type: did not coerce real to integer'
call h5f%finalize()

end subroutine test_wrong_type


subroutine test_unknown_write()
integer :: ierr
character(*), parameter :: filename = 'bad.5'
complex :: x

x = (1, -1)

call h5f%initialize(filename, ierr, status='replace', action='write', verbose=.false.)
if(ierr/=0) error stop 'test_unknown_write: creating file'
call h5f%write('/complex', x, ierr)
if(ierr==0) error stop 'test_unknown_write: writing unknown type variable'
end subroutine test_unknown_write


subroutine test_unknown_read()
integer :: ierr
character(*), parameter :: filename = 'bad.h5'
complex :: x

x = (1, -1)

call h5f%initialize(filename, status='unknown', action='readwrite', verbose=.false.)
call h5f%read('/complex', x, ierr)
if(ierr==0) error stop 'test_unknown_read: reading unknown type variable'
end subroutine test_unknown_read

end program
