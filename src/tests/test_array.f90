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
integer(int32), dimension(4,4) :: i2, i2t
integer(HSIZE_T), allocatable :: dims(:)
real(real32), allocatable :: rr2(:,:)
real(real32)  ::  nant,  r2(4,4)
integer :: i, ierr
integer(int32) :: i1(4), i2_8(8,8)
real(real32) :: r1(4)

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

call h5f%initialize(path//'/test.h5', ierr, status='old',action='rw',comp_lvl=1, chunk_size=[2,2,1,1,1,1,1])
if(ierr/=0) error stop 'initialize'
call h5f%write('/test/group2/ai2', i2, ierr)
if(ierr/=0) error stop 'write 2-D: int32'
call h5f%write('/test/real2', r2, ierr)
if(ierr/=0) error stop 'write 2-D: real32'
call h5f%write('/nan', nan, ierr)
if(ierr/=0) error stop 'write 0-D: real32 NaN'
call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

call h5f%initialize(path//'/test.h5', ierr,status='old',action='r')

! --- int32

call h5f%read('/test/group2/ai2',i2t, ierr)
if (.not.all(i2==i2t)) error stop 'read 2-D: int32 does not match write'

! verify reading into larger array
i2_8 = 0
call h5f%read('/test/group2/ai2', i2_8(2:5,3:6), ierr)
if (.not.all(i2_8(2:5,3:6) == i2)) error stop 'read into larger array fail'

! --- real

call h5f%shape('/test/real2',dims, ierr)
allocate(rr2(dims(1), dims(2)))
call h5f%read('/test/real2',rr2, ierr)
if (.not.all(r2 == rr2)) error stop 'real 2-D: read does not match write'

call h5f%read('/nan',nant, ierr)
if (.not.ieee_is_nan(nant)) error stop 'failed storing or reading NaN'
call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

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
if (ierr /= 0) error stop 'write initialize'

do i = 1,ng
write(ic,'(I2)') i
call h5f%write('/group'//trim(adjustl(ic))//'/flux_node',flux(:ng,i), ierr)
enddo

call h5f%read('/group1/flux_node',fo, ierr)
if (.not.all(fo(:ng)==flux(:ng,1))) error stop 'test_read_write: read does not match write'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine test_readwrite_array


end module test_array