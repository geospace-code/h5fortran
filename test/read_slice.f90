program read_slice
!! example of Fortran reading smaller array into slice of larger array via subroutine

implicit none

type :: T
integer :: i44(4,4)
end type T

integer :: i, bigA(4,4)
type(T) :: B

bigA = -1

call getter(bigA(2:3,3:4))

do i = 1,size(bigA,1)
  print '(4I3)', bigA(i,:)
enddo

! ----

B%i44 = -1

call getter(B%i44(2:3,3:4))

print *,''
do i = 1,size(B%i44,1)
  print '(4I3)', B%i44(i,:)
enddo

!! should print
!! -1 -1 -1 -1
!! -1 -1  1  2
!! -1 -1  3  4
!! -1 -1 -1 -1

contains

subroutine getter(A)
integer, intent(out) :: A(2,2)

A = reshape([1,2,3,4], shape(A), order=[2,1])

end subroutine getter

end program
