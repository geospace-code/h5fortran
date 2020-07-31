program test_attributes

use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit
use h5fortran, only: hdf5_file, h5write_attr, h5read_attr

implicit none (type, external)

character(:), allocatable :: path, filename
character(256) :: argv
integer :: i,l

call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif
path = trim(argv)
print *, 'test path: ', path

filename = path // '/test_attr.h5'


call test_write_attributes(filename)
call h5write_attr(filename, '/x', 'str29', '29')
call h5write_attr(filename, '/x', 'int29', [29])
print *,'PASSED: HDF5 write attributes'


call test_read_attributes(filename)
call h5read_attr(filename, '/x', 'str29', argv)
if (argv /= '29') error stop 'readattr_lt string'
block
  integer :: i32(1)
  call h5read_attr(filename, '/x', 'int29', i32)
  if (i32(1) /= 29) error stop 'readattr_lt integer'
end block

print *, 'PASSED: HDF5 read attributes'

contains

subroutine test_write_attributes(path)

type(hdf5_file) :: h
character(*), intent(in) :: path

call h%initialize(path)

call h%write('/x', 1)

call h%writeattr('/x', 'note','this is just a little number')
call h%writeattr('/x', 'hello', 'hi')
call h%writeattr('/x', 'life', [42])
call h%writeattr('/x', 'life_float', [42._real32, 84._real32])
call h%writeattr('/x', 'life_double', [42._real64])

call h%finalize()

end subroutine test_write_attributes


subroutine test_read_attributes(path)

type(hdf5_file) :: h
character(*), intent(in) :: path
character(1024) :: attr_str
integer :: attr_int(1)
real(real32) :: attr32(2)
real(real64) :: attr64(1)

integer :: x

call h%initialize(path)

call h%read('/x', x)
if (x/=1) error stop 'readattr: unexpected value'

call h%readattr('/x', 'note', attr_str)
if (attr_str /= 'this is just a little number') error stop 'readattr value note'

call h%readattr('/x', 'life', attr_int)
if (attr_int(1)/=42) error stop 'readattr: int'

call h%readattr('/x', 'life_float', attr32)
if (any(attr32/=[42._real32, 84._real32])) error stop 'readattr: real32'

call h%readattr('/x', 'life_double', attr64)
if (attr64(1)/=42._real64) error stop 'readattr: real64'

call h%finalize()

end subroutine test_read_attributes



end program
