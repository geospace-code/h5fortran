program repeat_read

use h5fortran, only : hdf5_file
implicit none

type(hdf5_file) :: h

integer :: i

character(:), allocatable :: filename
character(2) :: ct, c1
character(4) :: ds

!filename = '../h5fortran_example.h5'
filename = 'char.h5'
ct = "MA"

call h%open(filename, 'rw')

do i = 1,6
  write(ds,'(a1,i1,a2)') '/', i, '/a'
  call h%write(ds, ct)
enddo

call h%close()


call h%open(filename, 'r')
do i = 1,6
  write(ds,'(a1,i1,a2)') '/', i, '/a'
  call h%read(ds, c1)
  if (c1 /= ct) error stop "failed on read of dataset " // ds
enddo
call h%close()

print *, 'OK: repeat read'

end program
