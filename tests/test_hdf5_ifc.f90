!! unit tests and registration tests of HDF5 OO interface
use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
use, intrinsic:: iso_fortran_env, only: int64, int32, real32, real64, stderr=>error_unit
use, intrinsic:: iso_c_binding, only: c_null_char
use hdf5_interface, only: hdf5_file, toLower, strip_trailing_null

implicit none

type(hdf5_file) :: h5f
integer :: i1(4)
real(real32)    :: nan, r1(4), r2(4,4)

integer :: i

nan = ieee_value(1.0, ieee_quiet_nan)

do concurrent (i = 1:size(i1))
  i1(i) = i
enddo

r1 = i1

call test_lowercase()
print *,'PASSED: HDF5 character'
call test_strip_null()
print *,'PASSED: null strip'
call testNewHDF5()
print *,'PASSED: HDF5 scalar real / integer'
call testGroup()
print *,'PASSED: HDF5 group'
call testAddHDF5()
print *,'PASSED: HDF5 array'
call test_hdf5_deflate()
print *,'PASSED: HDF5 compression'
call test_write_attributes()
print *,'PASSED: HDF5 attributes'

call testrwHDF5(ng=69, nn=100, pn=5)
print *,'PASSED: HDF5 array write/read'

call test_string_rw()
print *,'PASSED: HDF5 string write/read'

print *,'OK: HDF5 h5fortran library'

contains


subroutine test_lowercase()

character(*), parameter :: hello = 'HeLl0 Th3rE !>? '
  !! Fortran 2003 allocatable string

if (.not.(toLower(hello)=='hell0 th3re !>? ')) error stop 'error: lowercase conversion'

if (.not.(trim(toLower(hello))=='hell0 th3re !>?')) error stop 'Allocatable lowercase conversion error'

if(.not.all(toLower(['Hi','hI'])==['hi','hi'])) error stop 'error on array conversion'

end subroutine test_lowercase


subroutine test_strip_null()

character(*), parameter :: hello = 'HeLl0 Th3rE !>? '

if (.not.strip_trailing_null(hello // c_null_char) == hello) error stop 'problem stripping trailing null'

end subroutine test_strip_null


subroutine testNewHDF5()

real(real32), allocatable :: rr1(:)
real(real32) :: rt
integer(int32) :: it
integer(int32), allocatable :: i1t(:)

call h5f%initialize('test.h5',status='new',action='w')

!! scalar tests
call h5f%add('/scalar_int', 42_int32)
call h5f%add('/scalar_real', 42._real32)

call h5f%get('/scalar_int', it)
call h5f%get('/scalar_real', rt)
if (.not.(rt==it .and. it==42)) then
  write(stderr,*) it,'/=',rt
  error stop 'scalar real / int: not equal 42'
endif

call h5f%add('/real1',r1)
call h5f%get('/real1',rr1)
if (.not.all(r1 == rr1)) error stop 'real: read does not match write'


call h5f%add('/ai1', i1)
call h5f%get('/ai1',i1t)

call h5f%finalize()

if (.not.all(i1==i1t)) error stop 'integer 1-D: read does not match write'

if (.not. h5f%filename == 'test.h5') error stop h5f%filename//' mismatch filename'

end subroutine testNewHDF5


subroutine testGroup


call h5f%initialize('test_groups.h5',status='new',action='rw')
call h5f%add('/test/')

call h5f%open('/test')
call h5f%add('group3/scalar', 1_int32)
call h5f%add('group3/scalar_real', 1._real32)
call h5f%close()

call h5f%finalize()

end subroutine testGroup


subroutine testAddHDF5()
!! tests that compression doesn't fail for very small datasets, where it really shouldn't be used (makes file bigger)

integer :: i2(4,4)
integer, allocatable :: i2t(:,:)
real(real32), allocatable :: rr2(:,:)
real(real32)  ::  nant

i2(1,:) = i1
do concurrent (i = 1:size(i2,2))
  i2(i,:) = i2(1,:) * i
enddo

r2 = i2

call h5f%initialize('test.h5',status='old',action='rw',comp_lvl=1)

call h5f%add('/test/group2/ai2', i2)
call h5f%get('/test/group2/ai2',i2t)
if (.not.all(i2==i2t)) error stop 'read does not match write'

call h5f%add('/test/real2',r2)
call h5f%get('/test/real2',rr2)
if (.not.all(r2 == rr2)) error stop 'real: read does not match write'

call h5f%add('/nan',nan)
call h5f%get('/nan',nant)
if (.not.ieee_is_nan(nant)) error stop 'failed storing or reading NaN'

call h5f%finalize()

end subroutine testAddHDF5


subroutine test_hdf5_deflate()

integer(int64), parameter :: N=1000
integer(int64) :: crat
integer ::  fsize, ibig2(N,N) = 0, ibig3(N,N,4) = 0
real(real32) :: big2(N,N) = 0., big3(N,N,4) = 0.


call h5f%initialize('test_deflate.h5',status='new',action='rw',comp_lvl=1)

call h5f%add('/big2', big2, chunk_size=[100,100])

call h5f%finalize()

inquire(file='test_deflate.h5', size=fsize)
crat = (N*N*storage_size(big2)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   2D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) write(stderr,*) 'warning: 2D low compression'

!======================================
call h5f%initialize('test_deflate.h5',status='new',action='rw',comp_lvl=1)

call h5f%add('/big3', big3, chunk_size=[100,100,1])

call h5f%finalize()

inquire(file='test_deflate.h5', size=fsize)
crat = (N*N*storage_size(big3)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) write(stderr,*) 'warning: 3D low compression'
!======================================
call h5f%initialize('test_deflate.h5',status='new',action='rw',comp_lvl=1)

call h5f%add('/ibig3', ibig3, chunk_size=[1000,100,1])

call h5f%finalize()

inquire(file='test_deflate.h5', size=fsize)
crat = (N*N*storage_size(ibig3)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) write(stderr,*) 'warning: 3D low compression'
!======================================
call h5f%initialize('test_deflate.h5',status='new',action='rw',comp_lvl=1)

call h5f%add('/ibig2', ibig2, chunk_size=[100,100])

call h5f%finalize()

inquire(file='test_deflate.h5', size=fsize)
crat = (N*N*storage_size(ibig2)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) write(stderr,*) 'warning: 3D low compression'

end subroutine test_hdf5_deflate


subroutine test_write_attributes()

call h5f%initialize('test_deflate.h5')

call h5f%writeattr('/little/','note','this is just a little number')

call h5f%finalize()

end subroutine test_write_attributes


subroutine test_string_rw()

character(2) :: value

call h5f%initialize('test_string.h5',status='new')

call h5f%add('/little','42')

call h5f%get('/little',value)

if (.not.value=='42') error stop 'string dataset read/write verification failure. Value: '// value

call h5f%finalize()

end subroutine test_string_rw


subroutine testrwHDF5(ng, nn, pn)
!! more group

integer, intent(in) :: ng, nn, pn

real(real32), allocatable :: flux(:,:),fo(:)
character(2) :: pnc,ic
integer :: i

allocate(flux(nn,ng),fo(nn))
flux = 1.0
write(pnc,'(I2)') pn

call h5f%initialize('p'//trim(adjustl(pnc))//'.h5',status='new',action='w')

do i = 1,ng
write(ic,'(I2)') i
call h5f%add('/group'//trim(adjustl(ic))//'/flux_node',flux(:ng,i))
enddo

call h5f%get('/group1/flux_node',fo)
if (.not.all(fo(:ng)==flux(:ng,1))) error stop 'test_read_write: read does not match write'

call h5f%finalize()


end subroutine testrwHDF5

end program
