module test_array

use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
use, intrinsic :: iso_fortran_env, only: real32, real64, int32
use h5fortran, only : hdf5_file, hsize_t

implicit none

real(real32) :: nan

contains

subroutine test_write_array(path)
!! tests that compression doesn't fail for very small datasets, where it really shouldn't be used (makes file bigger)
type(hdf5_file) :: h5f
character(*), intent(in) :: path

integer(int32), dimension(4) :: i1, i1t
integer(int32), dimension(4,4) :: i2, i2t
real(real32), allocatable :: rr2(:,:)
real(real32)  ::  nant, r1(4), r2(4,4)
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

!! write test data
call h5f%initialize(path//'/test.h5', status='old',action='rw',comp_lvl=1, chunk_size=[2,2,1,1,1,1,1])

call h5f%write('/int32-1d', i1)
call h5f%write('/test/group2/int32-2d', i2)
call h5f%write('/test/real2', r2)
call h5f%write('/nan', nan)

!> test writing wrong size
call h5f%write('/int32-1d', [-1], ierr)
if(ierr==0) error stop 'test_write_array: did not error for write array shape mismatch'

!> test writing wrong rank
call h5f%write('/int32-1d', i2, ierr)
if(ierr==0) error stop 'test_write_array: did not error for write array rank mismatch'

call h5f%finalize()

!! Read tests
call h5f%initialize(path//'/test.h5', ierr,status='old',action='r')
if(ierr/=0) error stop
!> int32

call h5f%read('/int32-1d', i1t, ierr)
if(ierr/=0) error stop
if (.not.all(i1==i1t)) error stop 'read 1-d int32: does not match write'

call h5f%read('/test/group2/int32-2d',i2t, ierr)
if(ierr/=0) error stop
if (.not.all(i2==i2t)) error stop 'read 2-D: int32 does not match write'

!> verify reading into larger array
i2_8 = 0
call h5f%read('/test/group2/int32-2d', i2_8(2:5,3:6), ierr)
if (.not.all(i2_8(2:5,3:6) == i2)) error stop 'read into larger array fail'

!> check error for reading array dimension mismatch

!> check that 1D disk into 2D raises error
call h5f%read('/int32-1d', i2, ierr)
if (ierr==0) error stop 'failed to error on read rank mismatch'

! --- real

block
  integer(HSIZE_T), allocatable :: dims(:)
  call h5f%shape('/test/real2',dims, ierr)
  if(ierr/=0) error stop
  allocate(rr2(dims(1), dims(2)))
  call h5f%read('/test/real2',rr2, ierr)
  if(ierr/=0) error stop
  if (.not.all(r2 == rr2)) error stop 'real 2-D: read does not match write'
end block

call h5f%read('/nan',nant, ierr)
if(ierr/=0) error stop
if (.not.ieee_is_nan(nant)) error stop 'failed storing or reading NaN'
call h5f%finalize(ierr)
if(ierr/=0) error stop

end subroutine test_write_array


subroutine test_readwrite_array(path,ng, nn, pn)
!! more group
type(hdf5_file) :: h5f
character(*), intent(in) :: path
integer, intent(in) :: ng, nn, pn

real(real32), allocatable :: flux(:,:),fo(:)
character(2) :: pnc,ic
integer :: i, ierr

allocate(flux(nn,ng),fo(nn))
flux = 1.0
write(pnc,'(I2)') pn

call h5f%initialize(path//'/p'//trim(adjustl(pnc))//'.h5', ierr, status='new',action='w')
if(ierr/=0) error stop

do i = 1,ng
  write(ic,'(I2)') i
  call h5f%write('/group'//trim(adjustl(ic))//'/flux_node',flux(:,i), ierr)
  if (ierr /= 0) error stop
enddo

call h5f%read('/group1/flux_node',fo, ierr)
if (ierr /= 0) error stop
if (.not.all(fo == flux(:,1))) error stop 'test_read_write: read does not match write'

call h5f%finalize(ierr)
if(ierr/=0) error stop

end subroutine test_readwrite_array


end module test_array