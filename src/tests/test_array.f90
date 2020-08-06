program array_test

use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
use, intrinsic :: iso_fortran_env, only: real32, real64, int32, stderr=>error_unit
use h5fortran, only : hdf5_file, hsize_t

implicit none (type, external)

real(real32) :: nan

call test_basic_array('test_array.h5')
print *,'PASSED: array write'
call test_read_slice('test_array.h5')
print *, 'PASSED: slice read'

call test_readwrite_array('test_group_array.f90', ng=69, nn=100, pn=5)
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
real(real32)  ::  nant, r1(4), r2(4,4), B(6,6)
integer :: i, ierr
integer(int32) :: i2_8(8,8)

nan = ieee_value(1.0, ieee_quiet_nan)

do i = 1,size(i1)
  i1(i) = i
enddo

i2(1,:) = i1
do i = 1,size(i2,2)
  i2(i,:) = i2(1,:) * i
enddo

r1 = i1
r2 = i2

call h%initialize(filename, status='replace', comp_lvl=1, verbose=.False.)

call h%write('/int32-1d', i1)
call h%write('/test/group2/int32-2d', i2)
call h%write('/real32-2d', r2)
call h%write('/nan', nan)

!> test writing wrong size
call h%write('/int32-1d', [-1], ierr=ierr)
if(ierr==0) error stop 'test_write_array: did not error for write array shape mismatch'

!> test writing wrong rank
call h%write('/int32-1d', i2, ierr=ierr)
if(ierr==0) error stop 'test_write_array: did not error for write array rank mismatch'

call h%finalize()

!! read
call h%initialize(filename, status='old', action='r', verbose=.false.)

!> int32
call h%read('/int32-1d', i1t)
if (.not.all(i1==i1t)) error stop 'read 1-d int32: does not match write'

call h%read('/test/group2/int32-2d',i2t)
if (.not.all(i2==i2t)) error stop 'read 2-D: int32 does not match write'

!> verify reading into larger array
i2_8 = 0
call h%read('/test/group2/int32-2d', i2_8(2:5,3:6))
if (.not.all(i2_8(2:5,3:6) == i2)) error stop 'read into larger array fail'

!> check error for reading array dimension mismatch

!> check that 1D disk into 2D raises error
call h%read('/int32-1d', i2, ierr)
if (ierr==0) error stop 'failed to error on read rank mismatch'

!> real
call h%shape('/real32-2d',dims)
allocate(rr2(dims(1), dims(2)))
call h%read('real32-2d',rr2)
if (.not.all(r2 == rr2)) error stop 'real 2-D: read does not match write'

! check read into a variable slice
call h%read('real32-2d', B(2:5,3:6))
if(.not.all(B(2:5,3:6) == r2)) error stop 'real 2D: reading into variable slice'

call h%read('/nan',nant)
if (.not.ieee_is_nan(nant)) error stop 'failed storing or reading NaN'

call h%finalize()

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

call h%initialize(filename, status='old', action='r')

print *, 'read slice 1d, stride=1'
i1t = 0
call h%read('/int32-1d', i1t(:2), istart=[2], iend=[3], stride=[1])
if (.not.all(i1t(:2)==[2,3])) then
  write(stderr, *) 'read 1D slice does not match. expected [2,3] but got ',i1t(:2)
  error stop
endif

print *, 'read slice 1d, no stride'
i1t = 0
call h%read('/int32-1d', i1t(:2), istart=[2], iend=[3])
if (.not.all(i1t(:2)==[2,3])) then
  write(stderr, *) 'read 1D slice does not match. expected [2,3] but got ',i1t(:2)
  error stop
endif

print *, 'read slice 2d, stride=1'
i2t = 0
call h%read('/test/group2/int32-2d', i2t(:2,:3), istart=[2,1], iend=[3,3], stride=[1,1])
if (.not.all(i2t(:2,:3)==i2(2:3,1:3))) then
  write(stderr, *) 'read 2D slice does not match. expected:',i2(2:3,1:3),' but got ',i2t(:2,:3)
  error stop
endif

call h%finalize()

end subroutine test_read_slice


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

call h%initialize(filename,  status='scratch')

do i = 1,ng
  write(ic,'(I2)') i
  call h%write('/group'//trim(adjustl(ic))//'/flux_node',flux(:,i))
enddo

call h%read('/group1/flux_node',fo)
if (.not.all(fo == flux(:,1))) error stop 'test_read_write: read does not match write'

call h%finalize()

end subroutine test_readwrite_array

end program
