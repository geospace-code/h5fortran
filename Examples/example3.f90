module example3

use, intrinsic :: iso_c_binding, only : C_LONG
use h5fortran, only : h5write, h5read

implicit none (type, external)

contains


subroutine write_int32(i32) bind(C)
integer(C_LONG), intent(in) :: i32

call h5write('h5fortran_example3.h5', '/x', i32)

end subroutine write_int32


subroutine read_int32(i32) bind(C)
integer(C_LONG), intent(out) :: i32

call h5read('h5fortran_example3.h5', '/x', i32)

end subroutine read_int32


end module example3
