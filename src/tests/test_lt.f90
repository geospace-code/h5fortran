module test_lt

use h5fortran, only : h5write, h5read

implicit none (external)

contains

subroutine test_readwrite_lt(path)

character(*), intent(in) :: path
integer :: ierr, L, L1(8), L2(2,1), L3(1,1,1), L4(1,1,1,1), L5(1,1,1,1,1), L6(1,1,1,1,1,1), L7(1,1,1,1,1,1,1)

L = 123
L2 = L; L3=L; L4=L; L5=L; L6=L; L7=L

call h5write(path//'/scalar_int.h5', '/int', 42)


call h5write(path//'/golt.h5', '/int32_0d', 121242, ierr)
if (ierr/=0) error stop 'lt file write'

call h5read(path//'/golt.h5', '/int32_0d', L)
if (L /= 121242) error stop 'incorrect read value'

! --- 1d

call h5write(path//'/golt.h5','/int32_1d', [1,2,3,4,5,6], ierr)
if (ierr/=0) error stop 'write 1d error'
L1 = 0
call h5read(path//'/golt.h5', '/int32_1d', L1(2:7), ierr)
if (ierr/=0) error stop 'read 1d error'
if(.not. all(L1(2:7) == [1,2,3,4,5,6])) error stop '1d slice read error'

! --- 2d

call h5write(path//'/golt.h5','/int32_2d', L2, ierr)
if (ierr/=0) error stop 'write 2d error'
call h5read(path//'/golt.h5', '/int32_2d', L2, ierr)
if (ierr/=0) error stop 'read 2d error'

! --- 3d

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
