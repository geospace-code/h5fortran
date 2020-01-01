use, intrinsic:: iso_fortran_env, only: int64, int32, real32, real64, stderr=>error_unit
use hdf5_interface, only: hdf5_file, HSIZE_T

implicit none

type(hdf5_file) :: h5f

character(:), allocatable :: path
character(256) :: argv
integer :: i

call get_command_argument(1, argv, status=i)
if (i/=0 .or. len_trim(argv) == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif

call test_nonexist_file(path)
print *, 'OK: non-existing file'
call test_nonhdf5_file(path)
print *, 'OK: non-HDF5 file'

contains


subroutine test_nonexist_file(path)
character(*), intent(in) :: path
integer :: ierr

call h5f%initialize(path//'/not-exist', ierr, status='old', action='read')
if (ierr==0) error stop 'should have had ierr/=0 on non-existing file'
end subroutine test_nonexist_file


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
call h5f%initialize(filename, ierr, status='replace', action='readwrite')
if(ierr/=0) error stop 'test_nonexist_variable: opening file'
call h5f%read('/not-exist', u, ierr)
if(ierr==0) error stop 'test_nonexist_variable: should have ierr/=0 on non-exist variable'
call h5f%finalize(ierr)
if (ierr/=0) error stop 'test_nonexist_variable: finalizing'

end subroutine test_nonexist_variable


end program