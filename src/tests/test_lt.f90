module test_lt

use h5fortran, only : h5write, h5read
implicit none
contains

subroutine test_readwrite_lt(path)

character(*), intent(in) :: path
integer :: ierr, L, L2(2,1), L3(1,1,1), L4(1,1,1,1), L5(1,1,1,1,1), L6(1,1,1,1,1,1), L7(1,1,1,1,1,1,1)

L = 123
L2 = L; L3=L; L4=L; L5=L; L6=L; L7=L

call h5write(path//'/scalar_int.h5', '/int', 42)


call h5write(path//'/golt.h5', '/int32_0d', 121242, ierr)
if (ierr/=0) error stop 'lt file write'

call h5read(path//'/golt.h5', '/int32_0d', L)
if (L /= 121242) error stop 'incorrect read value'

call h5write(path//'/golt.h5','/int32_2d', L2, ierr)
if (ierr/=0) error stop 'write 2d error'
call h5read(path//'/golt.h5', '/int32_2d', L2, ierr)
if (ierr/=0) error stop 'read 2d error'

call h5write(path//'/golt.h5','/int32_3d', L3, ierr)
if (ierr/=0) error stop 'write 3d error'
call h5read(path//'/golt.h5', '/int32_3d', L3, ierr)
if (ierr/=0) error stop 'read 3d error'

call h5write(path//'/golt.h5','/int32_4d', L4, ierr)
if (ierr/=0) error stop 'write 4d error'
call h5read(path//'/golt.h5', '/int32_4d', L4, ierr)
if (ierr/=0) error stop 'read 4d error'

call h5write(path//'/golt.h5','/int32_5d', L5, ierr)
if (ierr/=0) error stop 'write 5d error'
call h5read(path//'/golt.h5', '/int32_5d', L5, ierr)
if (ierr/=0) error stop 'read 5d error'

call h5write(path//'/golt.h5','/int32_6d', L6, ierr)
if (ierr/=0) error stop 'write 6d error'
call h5read(path//'/golt.h5', '/int32_6d', L6, ierr)
if (ierr/=0) error stop 'read 6d error'

call h5write(path//'/golt.h5','/int32_7d', L7, ierr)
if (ierr/=0) error stop 'write 7d error'
call h5read(path//'/golt.h5', '/int32_7d', L7, ierr)
if (ierr/=0) error stop 'read 7d error'

end subroutine test_readwrite_lt

end module test_lt