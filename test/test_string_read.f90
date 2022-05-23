program main

use hdf5, only: H5T_STR_NULLPAD_F, H5T_STR_NULLTERM_F
use h5fortran, only: hdf5_file

implicit none (type, external)

character(1000) :: pyp, vstr, fstr
integer :: i

type(hdf5_file) :: h

call get_command_argument(1, pyp, status=i)
if (i/=0) error stop "specify file to read"

call h%open(pyp, "r")

call h%read("/variable", vstr)
if(vstr /= "Hello World!") error stop "h5py read variable length failed: " // trim(vstr)

if (h%get_strpad("/variable") /= H5T_STR_NULLTERM_F) error stop "NULLTERM expected for /variable"


call h%read("/nullpad", fstr)
if(fstr /= "Hello World!") error stop "h5py read null pad failed: " // trim(fstr)

if (h%get_strpad("/nullpad") /= H5T_STR_NULLPAD_F) error stop "NULLPAD expected for /nullpad"

call h%close()

print *, "OK: variable/nullpad length string read"

end program
