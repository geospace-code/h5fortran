module test_string

use, intrinsic:: iso_fortran_env, only:  stderr=>error_unit
use, intrinsic:: iso_c_binding, only: c_null_char

use h5fortran, only : toLower, hdf5_file, strip_trailing_null, truncate_string_null

implicit none

contains

subroutine test_lowercase()

character(*), parameter :: hello = 'HeLl0 Th3rE !>? '
  !! Fortran 2003 allocatable string

if (.not.(toLower(hello)=='hell0 th3re !>? ')) error stop 'error: lowercase conversion'

if (.not.(trim(toLower(hello))=='hell0 th3re !>?')) error stop 'Allocatable lowercase conversion error'

end subroutine test_lowercase


subroutine test_strip_null()

character(*), parameter :: hello = 'HeLl0 Th3rE !>? '

if (.not.strip_trailing_null(hello // c_null_char) == hello) error stop 'problem stripping trailing null'

end subroutine test_strip_null


subroutine test_string_rw(path)

type(hdf5_file) :: h5f

character(*), intent(in) :: path
character(2) :: value
character(1024) :: val1k
character(:), allocatable :: final

call h5f%initialize(path//'/test_string.h5', status='new', action='rw')

call h5f%write('/little', '42')
call h5f%read('/little', value)

if (value /= '42') then
  write(stderr,*) 'test_string:  read/write verification failure. Value: '// value
  error stop
endif

!! try reading too much data, then truncating to first C_NULL
call h5f%read('/little', val1k)
final = truncate_string_null(val1k)

if (len(final) /= 2) then
  write(stderr, *) 'trimming str to c_null did not work, got len() = ', len(final)
  write(stderr, *) iachar(final(3:3))
  error stop
endif

call h5f%finalize()

end subroutine test_string_rw

end module test_string