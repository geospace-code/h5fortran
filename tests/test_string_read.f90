program main

use h5fortran, only : hdf5_file

implicit none (type, external)

character(1024) :: pyp, str
integer :: i

type(hdf5_file) :: h

call get_command_argument(1, pyp, status=i)
if (i/=0) error stop "specify file to read"

call h%open(pyp, "r")

call h%read("/str", str)

call h%close()

print *, len_trim(str), len(str)

if(str /= "Hello There!") error stop "h5py read failed: " // trim(str)


end program
