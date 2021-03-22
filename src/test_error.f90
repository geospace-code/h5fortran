program test_error

use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit
use h5fortran, only: hdf5_file

implicit none (type, external)

call test_nonexist_old_file()
print *, 'OK: non-existing old file'
call test_nonexist_unknown_file()
print *, 'OK: non-existing unknown file'
call test_nonhdf5_file()
print *, 'OK: non-HDF5 file'
call test_wrong_type()
print *, "OK: wrong type read"

contains


subroutine test_nonexist_old_file()
integer :: ierr
type(hdf5_file) :: h

call h%initialize('not-exist.h5', ierr, status='old', action='read', verbose=.false.)
if (ierr==0) error stop 'should have had ierr/=0 on non-existing old file'
end subroutine test_nonexist_old_file


subroutine test_nonexist_unknown_file()
integer :: ierr
type(hdf5_file) :: h

call h%initialize('not-exist.h5', ierr, status='unknown', action='read', verbose=.false.)
if (ierr==0) error stop 'should have had ierr/=0 on non-existing unknown read file'
end subroutine test_nonexist_unknown_file


subroutine test_nonhdf5_file()
integer :: u,ierr
type(hdf5_file) :: h
character(*), parameter :: filename = 'bad.h5'

! create or replace zero-length file, could be any size, just not a valid HDF5 file
open(newunit=u, file=filename, status='replace', action='write')
close(u)

call h%initialize(filename, ierr, status='old', action='read')
if (ierr==0) error stop 'should have had ierr/=0 on invalid HDF5 file'
end subroutine test_nonhdf5_file


subroutine test_wrong_type()
integer :: u
type(hdf5_file) :: h
character(*), parameter :: filename = 'bad.h5'

call h%initialize(filename, status='replace', verbose=.false.)
call h%write('/real32', 42.)
call h%finalize()

call h%initialize(filename, status='old', action='read', verbose=.false.)
call h%read('/real32', u)
if (u /= 42) error stop 'test_wrong_type: did not coerce real to integer'
call h%finalize()

end subroutine test_wrong_type

end program
