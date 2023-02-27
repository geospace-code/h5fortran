program exist_tests
!! test "exist" variable
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use h5fortran, only: hdf5_file, h5exist, is_hdf5, hdf5_close, H5T_NATIVE_INTEGER, hdf5_is_initialized

implicit none

call test_is_hdf5()
print *, 'OK: is_hdf5'

call test_exist('exist.h5')
print *, 'OK: exist'

call test_softlink('soft.h5')
print *, "OK: softlink"

call test_multifiles()
print *, 'OK: multiple files open at once'

contains


subroutine test_is_hdf5()
integer :: i

if(is_hdf5('apidfjpj-8j9ejfpq984jfp89q39SHf.h5')) error stop 'test_exist: non-existent file declared hdf5'

open(newunit=i, file='not_hdf5.h5', action='write')
write(i,*) 'I am not an HDF5 file.'
close(i)

if(is_hdf5('not_hdf5.h5')) error stop 'text files are not hdf5'

end subroutine test_is_hdf5


subroutine test_exist(fn)

type(hdf5_file) :: h

character(*), intent(in) :: fn

print '(a)', 'test_exist: creating file ' // fn
call h%open(fn, action='w', debug=.true.)

print '(a)', "creating test dataset /x"
call h%create('/x', H5T_NATIVE_INTEGER, [0])

print '(a)', 'test_exist: closing file ' // fn
call h%close()
if(.not.is_hdf5(fn)) error stop 'file does not exist'

print *, "test_exist: attemping to open file: ", fn
call h%open(fn, action='r', debug=.true.)
if (.not. h%exist('/x')) error stop 'x exists'

if (h%exist('/A')) error stop 'variable /A should not exist in ' // h%filename

call h%close()

print '(a,L1)', "test_exist: library still initialized: ", hdf5_is_initialized()

if(h%is_open()) error stop 'file is closed'

print '(a)', "test_exist: check h5exist() on file: " // fn
if (.not. h5exist(fn, '/x', debug=.true.)) error stop 'x exists'
if (h5exist(fn, '/A', debug=.true.)) error stop 'A not exist'

end subroutine test_exist


subroutine test_softlink(fn)

type(hdf5_file) :: h
character(*), intent(in) :: fn

integer :: y

call h%open(fn, action='w')

call h%write("/actual", 142)
call h%softlink("/actual", "/additional")
call h%read("/additional", y)

if (.not.h%exist("/additional")) error stop "softlink not present"

if (y /= 142) error stop "did not read softlink correctly"

!> test dangling link

call h%softlink("/not_here", "/not_yet")
if (h%exist("/not_yet")) error stop "dangling softlink"

call h%write("/not_here", 36)
call h%read("/not_yet", y)
if (y /= 36)  error stop "finalizing dangling link failed"

call h%close()

end subroutine test_softlink


subroutine test_multifiles()

type(hdf5_file) :: f,g,h

call f%open(filename='A.h5', action='w')
call g%open(filename='B.h5', action='w')
if (h%is_open()) error stop 'is_open() not isolated at constructor'
call h%open(filename='C.h5', action='w')

call f%flush()

call f%close()
if (.not.g%is_open() .or. .not. h%is_open()) error stop 'is_open() not isolated at destructor'
call g%close()
call h%close()

call hdf5_close()

end subroutine test_multifiles

end program
