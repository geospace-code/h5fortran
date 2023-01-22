program array_test

use, intrinsic :: iso_fortran_env, only: real32, real64, int32, stderr=>error_unit

use h5fortran, only : hdf5_file, HSIZE_T, H5T_NATIVE_INTEGER

implicit none

call test_basic_array('test_array.h5')
print *, 'PASSED: array write'

call test_read_slice('test_array.h5')
print *, 'PASSED: slice read'

call test_write_slice('test_array.h5')
print *, 'PASSED: slice write'

call test_readwrite_array('test_group_array.h5', ng=69, nn=100, pn=5)
print *,'PASSED: array write / read'


contains

subroutine test_basic_array(filename)

character(*), intent(in) :: filename
!! tests that compression doesn't fail for very small datasets, where it really shouldn't be used (makes file bigger)
type(hdf5_file) :: h
integer(HSIZE_T), allocatable :: dims(:)

integer(int32), dimension(4) :: i1, i1t
integer(int32), dimension(4,4) :: i2, i2t
real(real32), allocatable :: rr2(:,:)
real(real32)  ::  r1(4), r2(4,4), B(6,6)
integer :: i
integer(int32) :: i2_8(8,8)

do i = 1,size(i1)
  i1(i) = i
enddo

i2(1,:) = i1
do i = 1,size(i2,2)
  i2(i,:) = i2(1,:) * i
enddo

i2_8(3:6, 4:7) = i2

r1 = i1
r2 = i2

call h%open(filename, action='w', comp_lvl=1)

call h%write('/int32-1d', i1)
call h%write('/test/group2/int32-2d', i2)
call h%write('/real32-2d', r2)
!> write from subarray
call h%write('/sub-int32-2d', i2_8(3:6, 4:7))
print '(4i3)', i2_8(3:6, 4:7)

call h%close()

!! read
call h%open(filename, action='r')

!> int32
call h%read('/int32-1d', i1t)
if (.not.all(i1==i1t)) error stop 'read 1-d int32: does not match write'

call h%read('/test/group2/int32-2d',i2t)
if (.not.all(i2==i2t)) error stop 'read 2-D: int32 does not match write'

!> verify reading into larger array
i2_8 = 0
call h%read('/test/group2/int32-2d', i2_8(2:5,3:6))
if (.not.all(i2_8(2:5,3:6) == i2)) then
  write(stderr, '(a,4i3)') 'read 2-D: int32 slice does not match write: ', i2_8(2:5,3:6)
  error stop 'ERROR: read into larger array fail'
endif

!> verify reading written subarray
call h%read('/sub-int32-2d', i2t)
if (.not.all(i2==i2t)) then
write(stderr, '(a,4i3)') 'read 2-D: int32 write subarray slice does not match: ', i2t
error stop 'ERROR: read into larger array fail'
endif

!> real
call h%shape('/real32-2d',dims)
allocate(rr2(dims(1), dims(2)))
call h%read('real32-2d',rr2)
if (.not.all(r2 == rr2)) error stop 'real 2-D: read does not match write'

! check read into a variable slice
call h%read('real32-2d', B(2:5,3:6))
if(.not.all(B(2:5,3:6) == r2)) error stop 'real 2D: reading into variable slice'

call h%close()

end subroutine test_basic_array


subroutine test_read_slice(filename)

character(*), intent(in) :: filename

type(hdf5_file) :: h
integer :: i
integer(int32), dimension(4) :: i1, i1t
integer(int32), dimension(4,4) :: i2, i2t

do i = 1,size(i1)
  i1(i) = i
enddo

i2(1,:) = i1
do i = 1,size(i2,2)
  i2(i,:) = i2(1,:) * i
enddo

call h%open(filename, action='r')

i1t = 0
call h%read('/int32-1d', i1t(:2), istart=[2], iend=[3], stride=[1])
if (any(i1t(:2) /= [2,3])) then
  write(stderr, *) 'read 1D slice does not match. expected [2,3] but got ',i1t(:2)
  error stop
endif

i1t = 0
call h%read('/int32-1d', i1t(:2), istart=[2], iend=[3])
if (any(i1t(:2) /= [2,3])) then
  write(stderr, *) 'read 1D slice does not match. expected [2,3] but got ',i1t(:2)
  error stop
endif

i2t = 0
call h%read('/test/group2/int32-2d', i2t(:2,:3), istart=[2,1], iend=[3,3], stride=[1,1])
if (any(i2t(:2,:3) /= i2(2:3,1:3))) then
  write(stderr, *) 'read 2D slice does not match. expected:',i2(2:3,1:3),' but got ',i2t(:2,:3)
  error stop
endif

call h%close()

end subroutine test_read_slice


subroutine test_write_slice(filename)

character(*), intent(in) :: filename

type(hdf5_file) :: h
integer(int32), dimension(4) :: i1t
integer(int32), dimension(4,4) :: i2t
integer :: dims(1), dims2(2)

dims = [3]

call h%open(filename, action='r+', debug=.true.)

call h%create('/int32a-1d', dtype=H5T_NATIVE_INTEGER, dset_dims=dims)
call h%write('/int32a-1d', [1,3], istart=[1], iend=[2])
print *, 'PASSED: create dataset and write slice 1D'

call h%write('/int32-1d', [35, 70], istart=[2], iend=[3], stride=[1])

call h%read('/int32-1d', i1t)
if (.not.all(i1t==[1,35,70,4])) then
  write(stderr, *) 'write 1D slice does not match. Got ',i1t, ' in ', filename
  error stop
endif
print *, 'PASSED: overwrite slice 1d, stride=1'

call h%write('/int32-1d', [23,34,45], istart=[2], iend=[4])
call h%read('/int32-1d', i1t)
if (.not.all(i1t==[1,23,34,45])) then
  write(stderr, *) 'read 1D slice does not match. Got ',i1t
  error stop
endif
print *, 'PASSED: overwrite slice 1d, no stride'

dims2 = [4,4]

call h%create('/int32a-2d', dtype=H5T_NATIVE_INTEGER, dset_dims=dims2)
print *, 'create and write slice 2d, stride=1'
call h%write('/int32a-2d', reshape([76,65,54,43], [2,2]), istart=[2,1], iend=[3,2])

call h%read('/int32a-2d', i2t)

call h%close()


end subroutine test_write_slice


subroutine test_readwrite_array(filename, ng, nn, pn)
!! more group
type(hdf5_file) :: h
character(*), intent(in) :: filename
integer, intent(in) :: ng, nn, pn

real(real32), allocatable :: flux(:,:),fo(:)
character(2) :: pnc,ic
integer :: i

allocate(flux(nn,ng),fo(nn))
flux = 1.0
write(pnc,'(I2)') pn

call h%open(filename, action='rw')

do i = 1,ng
  write(ic,'(I2)') i
  call h%write('/group'//trim(adjustl(ic))//'/flux_node',flux(:,i))
enddo

call h%read('/group1/flux_node',fo)
if (.not.all(fo == flux(:,1))) error stop 'test_read_write: read does not match write'

call h%close()

end subroutine test_readwrite_array

end program
