module fortran_interface
!! filename(256) and var_name(64) are arbitrary sizes.
!! ensure calling program buffers are equally sized
use, intrinsic :: iso_c_binding, only : C_INT32_T, C_CHAR, C_NULL_CHAR
use h5fortran, only : h5write, h5read

implicit none

contains


subroutine write_int32(filename, var_name, i32) bind(C)
character(kind=C_CHAR) :: filename(256)
character(kind=C_CHAR) :: var_name(64)
integer(C_INT32_T), intent(in) :: i32

call h5write(cstr2fstr(filename), cstr2fstr(var_name), i32)

end subroutine write_int32


subroutine read_int32(filename, var_name, i32) bind(C)
character(kind=C_CHAR) :: filename(256)
character(kind=C_CHAR) :: var_name(64)
integer(C_INT32_T), intent(out) :: i32

call h5read(cstr2fstr(filename), cstr2fstr(var_name), i32)

end subroutine read_int32


function cstr2fstr(c_str) result(f_str)

character(kind=C_CHAR), intent(in) :: c_str(:)
character(len=size(c_str)) :: buf
character(:), allocatable :: f_str
integer :: i

buf = ""
!! clean variable, will get extra garbled text otherwise, maybe blank but non-trimmable character

do i = 1, size(c_str)
  if (c_str(i) == C_NULL_CHAR) exit
  buf(i:i) = c_str(i)
enddo

f_str = trim(buf)

end function cstr2fstr

end module fortran_interface
