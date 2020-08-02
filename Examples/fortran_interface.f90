module fortran_interface

use, intrinsic :: iso_c_binding, only : C_INT32_T, C_CHAR, C_NULL_CHAR
use h5fortran, only : h5write, h5read

implicit none (type, external)

contains


subroutine write_int32(filename, i32) bind(C)
character(kind=C_CHAR) :: filename(256)
integer(C_INT32_T), intent(in) :: i32

call h5write(cstr2fstr(filename), '/x', i32)

end subroutine write_int32


subroutine read_int32(filename, i32) bind(C)
character(kind=C_CHAR) :: filename(256)
integer(C_INT32_T), intent(out) :: i32

call h5read(cstr2fstr(filename), '/x', i32)

end subroutine read_int32


function cstr2fstr(c_str) result(f_str)

character(kind=C_CHAR), intent(in) :: c_str(:)
character(len=size(c_str)) :: f_str
integer :: i

do i = 1, size(c_str)
  if (c_str(i) == C_NULL_CHAR) exit
  f_str(i:i) = c_str(i)
enddo

end function cstr2fstr

end module fortran_interface
