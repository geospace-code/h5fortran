program test_attributes

use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit
use, intrinsic:: iso_c_binding, only: C_NULL_CHAR

use h5fortran, only: hdf5_file, h5write_attr, h5read_attr, HSIZE_T

implicit none

character(*), parameter :: filename = 'test_attr.h5'
character(8) :: s32  !< arbitrary length
integer :: i32(1)

call test_write_attributes(filename)

call h5write_attr(filename, '/x', 'str29', '29')
call h5write_attr(filename, '/x', 'int29', [29])
print *,'PASSED: HDF5 write attributes'

call test_read_attributes(filename)

call h5read_attr(filename, '/x', 'str29', s32)
if (s32 /= '29') error stop 'readattr_lt string'

call h5read_attr(filename, '/x', 'int29', i32)
if (i32(1) /= 29) error stop 'readattr_lt integer'

print *, 'PASSED: HDF5 read attributes'

contains

subroutine test_write_attributes(path)

type(hdf5_file) :: h
character(*), intent(in) :: path

character(123) :: buf

integer :: i2(1,1), i3(1,1,1), i4(1,1,1,1), i5(1,1,1,1,1), i6(1,1,1,1,1,1), i7(1,1,1,1,1,1,1)

call h%open(path, action='w', debug=.true.)

call h%write('/x', 1)

call h%writeattr('/x', 'int32-scalar', 42)
call h%writeattr('/x', 'real32-scalar', 42.0)
call h%writeattr('/x', 'char','this is just a little number')
call h%writeattr('/x', 'hello', 'hi')
call h%writeattr('/x', 'real32_1d', [real(real32) :: 42, 84])
call h%writeattr('/x', 'real64_1d0', [42._real64])

call h%writeattr('/x', 'i2', i2)
call h%writeattr('/x', 'i3', i3)
call h%writeattr('/x', 'i4', i4)
call h%writeattr('/x', 'i5', i5)
call h%writeattr('/x', 'i6', i6)
call h%writeattr('/x', 'i7', i7)

buf = ""
!! This auxiliary buffer fixes GCC 11.3.0 and oneAPI.
!! It wasn't necessary on GCC 11.4.1 and newer.
!! with h5dump, what's seen on disk file is "\001" value instead of "\000" or space.
call h%writeattr("/x", "empty_char", buf)

call h%writeattr("/x", "1d_empty", [character(1) :: "", ""])
call h%writeattr("/x", "c1d", [character(5) :: 'one', 'two', 'three'])
call h%writeattr("/x", "c2d", reshape([character(5) :: 'one', 'two', 'three', 'four', 'five', 'six'], [2,3]))


call h%close()

!> test overwrite of attributes
call h%open(path, action='a')
call h%writeattr('/x', 'int32-scalar', 142)
call h%writeattr('/x', 'real32_1d', [real(real32) :: 142, 84])
call h%writeattr('/x', 'char', 'overwrite attrs')
call h%delete_attr('/x', 'hello')
call h%close()

end subroutine test_write_attributes


subroutine test_read_attributes(path)

type(hdf5_file) :: h
character(*), intent(in) :: path
character(1024) :: attr_str
integer :: int32_0
real(real32) :: attr32(2)
real(real64) :: attr64
integer(HSIZE_T), allocatable :: dims(:)

character(5), allocatable :: c1d(:)

integer :: x

integer :: i2(1,1), i3(1,1,1), i4(1,1,1,1), i5(1,1,1,1,1), i6(1,1,1,1,1,1), i7(1,1,1,1,1,1,1)

call h%open(path, action='r')

call h%read('/x', x)
if (x/=1) error stop 'readattr: unexpected value'

!> character scalar
call h%readattr('/x', 'char', attr_str)
if (attr_str /= 'overwrite attrs') error stop 'overwrite attrs failed: ' // attr_str

call h%readattr("/x", "empty_char", attr_str)
print *, trim(attr_str) == c_null_char
if (len_trim(attr_str) /= 0) then
    write(stderr, '(a,i0)') "empty char attribute: expected 0 length, got length: ", len_trim(attr_str)
    error stop "empty char attribute failed"
endif
if (trim(attr_str) /= "") error stop "empty char attribute failed, got: " // trim(attr_str)

!> scalar numbers
call h%readattr('/x', 'int32-scalar', int32_0)
if (int32_0 /= 142) error stop 'readattr: int32-scalar'

call h%readattr('/x', 'real32_1d', attr32)
if (any(attr32 /= [real(real32) :: 142, 84])) error stop 'readattr: real32'

call h%readattr('/x', 'real64_1d0', attr64)
if (attr64 /= 42._real64) error stop 'readattr: real64'

!> casting
call h%readattr('/x', 'real32-scalar', int32_0)
if(int32_0 /= 42) error stop "readattr cast real to int"

call h%readattr('/x', 'int32-scalar', attr64)
if(attr64 /= 142) error stop "readattr cast int to real"

if (h%exist_attr('/x', 'hello')) error stop "delete attr failed"

call h%readattr('/x', 'i2', i2)
call h%readattr('/x', 'i3', i3)
call h%readattr('/x', 'i4', i4)
call h%readattr('/x', 'i5', i5)
call h%readattr('/x', 'i6', i6)
call h%readattr('/x', 'i7', i7)

!> character array

call h%shape("/x", dims, "c1d")
if(dims(1) /= 3) error stop "attr % shape: c1d"

allocate(c1d(dims(1)))
call h%readattr('/x', 'c1d', c1d)
if(c1d(1) /= 'one') error stop "attr:char:1d: index=1 " // c1d(1)
if(c1d(2) /= 'two') error stop "attr:char:1d: index=2 " // c1d(2)
if(c1d(3) /= 'three') error stop "attr:char:1d: index=3 " // c1d(3)

call h%close()

end subroutine test_read_attributes

end program
