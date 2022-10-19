program test_lt

use h5fortran, only : h5write, h5read

implicit none

integer :: L, L1(8), L2(2,1), L3(1,1,1), L4(1,1,1,1), L5(1,1,1,1,1), L6(1,1,1,1,1,1), L7(1,1,1,1,1,1,1)

L = 121242
L2 = L; L3=L; L4=L; L5=L; L6=L; L7=L

call h5write('scalar_int.h5', '/int', 42)
call h5write('golt.h5', '/int32_0d', 121242)

call h5read('golt.h5', '/int32_0d', L)
if (L /= 121242) error stop 'incorrect read value'

! --- 1d

call h5write('golt.h5','/int32_1d', [1,2,3,4,5,6])
L1 = 0
call h5read('golt.h5', '/int32_1d', L1(2:7))
if(.not. all(L1(2:7) == [1,2,3,4,5,6])) error stop '1d slice read error'

! --- 2d

call h5write('golt.h5','/int32_2d', L2)
call h5read('golt.h5', '/int32_2d', L2)

! --- 3d

call h5write('golt.h5','/int32_3d', L3)
call h5read('golt.h5', '/int32_3d', L3)

call h5write('golt.h5','/int32_4d', L4)
call h5read('golt.h5', '/int32_4d', L4)

call h5write('golt.h5','/int32_5d', L5)
call h5read('golt.h5', '/int32_5d', L5)

call h5write('golt.h5','/int32_6d', L6)
call h5read('golt.h5', '/int32_6d', L6)

call h5write('golt.h5','/int32_7d', L7)
call h5read('golt.h5', '/int32_7d', L7)

end program
