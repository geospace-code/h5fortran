program fill

use, intrinsic:: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_finite

use h5fortran, only : hdf5_file
use hdf5, only : H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER

implicit none

type(hdf5_file) :: h5

character(*), parameter :: fn = "test_fill.h5"

real :: NaN, r(3)
integer :: i(2)
character(10) :: c, fill_value

NaN = ieee_value(0., ieee_quiet_nan)

call h5%open(fn, "w")

call h5%create("/r32", H5T_NATIVE_REAL, dset_dims=[3], fill_value=NaN)
call h5%create("/r64", H5T_NATIVE_DOUBLE, dset_dims=[3], fill_value=NaN)
call h5%create("/i32", H5T_NATIVE_INTEGER, dset_dims=[2], fill_value=-1)

!> Note that character fill value must have same length as dataset, hence we use a character(10)
fill_value = "NaN"
call h5%create("/char", H5T_NATIVE_CHARACTER, dset_dims=[1], fill_value=fill_value, charlen=10)
fill_value = ""
call h5%create("/char_blank", H5T_NATIVE_CHARACTER, dset_dims=[1], fill_value=fill_value, charlen=10)
fill_value = " "
call h5%create("/char_space", H5T_NATIVE_CHARACTER, dset_dims=[1], fill_value=fill_value, charlen=10)

call h5%close()

call h5%open(fn, "r")

call h5%read("/r32", r)
if(any(ieee_is_finite(r))) error stop "real32: expected all NaN"

call h5%read("/r64", r)
if(any(ieee_is_finite(r))) error stop "real64: expected all NaN"

call h5%read("/i32", i)
if(any(i /= -1)) error stop "int32: expected all -1"

call h5%read("/char", c)
if(c /= "NaN") error stop "char: expected 'NaN', got: " // c

call h5%read("/char_blank", c)
if(c /= "") error stop "char: expected '', got: " // c

call h5%read("/char_space", c)
if(c /= " ") error stop "char: expected ' ', got: " // c

call h5%close()

print *, "OK: fill value"

end program
