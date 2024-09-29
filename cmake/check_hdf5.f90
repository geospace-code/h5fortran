program main

use hdf5

implicit none

integer :: i
integer(HID_T) :: fid

call H5open_f(i)
if(i /= 0) error stop "H5open_f failed [0]"

call H5open_f(i)
if(i /= 0) error stop "H5open_f failed [1]"

print '(a,i0)', "H5F_ACC_RDONLY_F = ", H5F_ACC_RDONLY_F
print '(a,i0)', "H5F_ACC_TRUNC_F = ", H5F_ACC_TRUNC_F
print '(a,i0)', "H5F_ACC_RDWR_F = ", H5F_ACC_RDWR_F

if(H5F_ACC_RDONLY_F == H5F_ACC_TRUNC_F .or. H5F_ACC_RDONLY_F == H5F_ACC_RDWR_F) then
    error stop "H5F_ACC_RDONLY, H5F_ACC_TRUNC, H5F_ACC_RDWR are not all distinct"
endif

call H5close_f(i)
if (i /= 0) error stop "H5close() failed"

print '(a)', "OK: HDF5 C type check"

end program
