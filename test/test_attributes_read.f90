program main

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

use h5fortran, only: hdf5_file, HSIZE_T

implicit none

character(1000) :: pyp, vstr, fstr
character(4), parameter :: smiley = "😀", wink = "😉"
character(4) :: u1

character(:), allocatable :: dn
character(20), allocatable :: c1d(:), c2d(:,:)

integer(HSIZE_T), allocatable :: dims(:)

integer :: i

type(hdf5_file) :: h

dn = "/empty"

call get_command_argument(1, pyp, status=i)
if (i/=0) error stop "specify file to read"

call h%open(pyp, "r")

call h%readattr(dn, "variable_str", vstr)
if(vstr /= "Hi there") error stop "h5py read_attr variable length failed: " // trim(vstr)

call h%shape(dn, dims, "variable_str1")
allocate(c1d(dims(1)))
call h%readattr(dn, "variable_str1", c1d)
if(c1d(1) /= "eight") then
  write(stderr,*) "ERROR: read_attr var. length index 1: " // trim(c1d(1)) // " len: ", len(c1d(1)), len_trim(c1d(1)), dims(1)
  error stop
endif
if(c1d(2) /= "nine") error stop "h5py read_attr variable length failed index 2: " // trim(c1d(2))

call h%shape(dn, dims, "variable_str2")
allocate(c2d(dims(1), dims(2)))
call h%readattr(dn, "variable_str2", c2d)
if(c2d(1, 1) /= "eight") then
  write(stderr,*) "ERROR: read_attr var. length index 1,1: " // trim(c2d(1, 1)) // " len: ", len(c2d(1,1)), len_trim(c2d(1,1))
  error stop
endif
if(c2d(3, 2) /= "thirteen") error stop "h5py read_attr variable length failed index 3,2: " // trim(c2d(3,2))

call h%readattr(dn, "nullpad", fstr)
if(fstr /= "Hello World!") error stop "h5py read null pad failed: " // trim(fstr)

!> 1-D array of character fixed string
call h%shape(dn, dims, "nullpad1")
deallocate(c1d)
allocate(c1d(dims(1)))
call h%readattr(dn, "nullpad1", c1d)
if(c1d(1) /= "two") error stop "h5py read null pad failed index 1: " // trim(c1d(1))
if(c1d(2) /= "three") error stop "h5py read null pad failed index 2: " // trim(c1d(2))
if(c1d(3) /= "four") error stop "h5py read null pad failed index 3: " // trim(c1d(3))

!> 2-D array of character fixed string
call h%shape(dn, dims, "nullpad2")
deallocate(c2d)
allocate(c2d(dims(1), dims(2)))
call h%readattr(dn, "nullpad2", c2d)
if(c2d(1,1) /= "eight") error stop "h5py read null pad failed index 1,1: " // trim(c2d(1,1))
if(c2d(3,2) /= "thirteen") error stop "h5py read null pad failed index 3,2: " // trim(c2d(3,2))

call h%readattr(dn, "smiley", u1)
print '(a)', "smiley: " // u1
if(u1 /= smiley) error stop "test_utf8: smiley failed"

call h%readattr(dn, "wink", u1)
print '(a)', "wink: " // u1
if(u1 /= wink) error stop "test_utf8: wink failed"


call h%close()

print *, "OK: variable/nullpad length string read"

end program
