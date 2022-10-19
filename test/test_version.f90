program test_version
!! tests that HDF5 library version is available

use h5fortran, only : hdf5version

implicit none

character(24) :: vstr
integer :: v(3), i, j, k, c

v = hdf5version()

print '(i0,a1,i0,a1,i0)', v(1), '.', v(2), '.', v(3)

if(command_argument_count() < 1) stop

call get_command_argument(1, vstr, status=i)
if (i/=0) error stop "input version string to compare"

k = index(vstr, '.')
read(vstr(:k-1), '(I3)') c
if(c /= v(1)) error stop "Major version mismatch: " // vstr(:k-1)
j = index(vstr(k+1:), '.')
read(vstr(k+1:k+j-1), '(I3)') c
if(c /= v(2)) error stop "Minor version mismatch: " // vstr(k+1:k+j-1)
i = scan(vstr(k+j+1:), '.-_')
if (i == 0) then
  read(vstr(k+j+1:), '(I3)') c
else
  read(vstr(k+j+1:k+j+i-1), '(I3)') c
end if
if(c /= v(3)) error stop "Release version mismatch: " // vstr(k+j+1:)

end program
