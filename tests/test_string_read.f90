program main

use h5fortran, only : hdf5_file

implicit none (type, external)

character(1000) :: pyp, str
integer :: i

type(hdf5_file) :: h

call get_command_argument(1, pyp, status=i)
if (i/=0) error stop "specify file to read"

call h%open(pyp, "r")

call h%read("/str", str)

call h%close()

if(str /= "Hello World!") error stop "h5py read failed: " // trim(str)

print *, "OK: variable length string read"

end program
