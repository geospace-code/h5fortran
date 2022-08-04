program main

!use hdf5, only: H5T_STR_NULLPAD_F, H5T_STR_NULLTERM_F
use h5fortran, only: hdf5_file

implicit none (type, external)

character(1000) :: pyp, vstr, fstr
character(4), parameter :: smiley = "😀", wink = "😉"
character(4) :: u1

character(:), allocatable :: dn


integer :: i

type(hdf5_file) :: h

dn = "/empty"

call get_command_argument(1, pyp, status=i)
if (i/=0) error stop "specify file to read"

call h%open(pyp, "r")

call h%readattr(dn, "variable_str", vstr)
if(vstr /= "Hi there") error stop "h5py read_attr variable length failed: " // trim(vstr)
!if (h%get_strpad(dn, "variable_str") /= H5T_STR_NULLTERM_F) error stop "NULLTERM expected for /variable_str"

call h%readattr(dn, "nullpad", fstr)
if(fstr /= "Hello World!") error stop "h5py read null pad failed: " // trim(fstr)
!if (h%get_strpad(dn, "nullpad") /= H5T_STR_NULLPAD_F) error stop "NULLPAD expected for /nullpad"

call h%readattr(dn, "smiley", u1)
print '(a)', "smiley: " // u1
if(u1 /= smiley) error stop "test_utf8: smiley failed"

call h%readattr(dn, "wink", u1)
print '(a)', "wink: " // u1
if(u1 /= wink) error stop "test_utf8: wink failed"


call h%close()

print *, "OK: variable/nullpad length string read"

end program
