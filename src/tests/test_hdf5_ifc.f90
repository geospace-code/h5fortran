!! unit tests and registration tests of HDF5 OO interface
use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
use, intrinsic:: iso_fortran_env, only: int64, int32, real32, real64, stderr=>error_unit
use, intrinsic:: iso_c_binding, only: c_null_char
use h5fortran, only: hdf5_file, toLower, strip_trailing_null, truncate_string_null, HSIZE_T

implicit none

type(hdf5_file) :: h5f
integer :: i1(4), ierr
real(real32)    :: nan, r1(4), r2(4,4)

character(:), allocatable :: path
character(256) :: argv
integer :: i,l

call get_command_argument(1, argv, length=l, status=i)
if (i /= 0 .or. l == 0) then
  write(stderr,*) 'please specify test directory e.g. /tmp'
  error stop 77
endif
path = trim(argv)
print *, 'test path: ', path

nan = ieee_value(1.0, ieee_quiet_nan)

do i = 1,size(i1)
  i1(i) = i
enddo

r1 = i1

call test_string_rw(path)
print *,'PASSED: HDF5 string write/read'

call test_lowercase()
print *,'PASSED: HDF5 character'
call test_strip_null()
print *,'PASSED: null strip'
call testNewHDF5(path)
print *,'PASSED: HDF5 scalar real and integer'
call testGroup(path)
print *,'PASSED: HDF5 group'
call testwriteHDF5(path)
print *,'PASSED: HDF5 array'
call test_hdf5_deflate(path)
print *,'PASSED: HDF5 compression'
call test_write_attributes(path)
print *,'PASSED: HDF5 attributes'

call testrwHDF5(path, ng=69, nn=100, pn=5)
print *,'PASSED: HDF5 array write / read'

call test_writeExistingVariable(path)
print *,'PASSED: write existing variable'

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


subroutine testNewHDF5(path)
character(*), intent(in) :: path
real(real32), allocatable :: rr1(:)
real(real32) :: rt
integer(int32) :: it
integer(int32), allocatable :: i1t(:)
integer(HSIZE_T), allocatable :: dims(:)

call h5f%initialize(path//'/test.h5', ierr, status='new',action='w')

!! scalar tests
call h5f%write('/scalar_int', 42_int32, ierr)
if (ierr /= 0) error stop 'write 0-D: int'

call h5f%write('/scalar_real', 42._real32, ierr)
if (ierr /= 0) error stop 'write 0-D: real32'

call h5f%write('/real1',r1, ierr)
if (ierr /= 0) error stop 'write 1-D: real32'

call h5f%write('/ai1', i1, ierr)
if (ierr /= 0) error stop 'write 1-D: scalar int'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

call h5f%initialize(path//'/test.h5', ierr, status='old',action='r')
call h5f%read('/scalar_int', it, ierr)
call h5f%read('/scalar_real', rt, ierr)
if (.not.(rt==it .and. it==42)) then
  write(stderr,*) it,'/=',rt
  error stop 'scalar real / int: not equal 42'
endif

call h5f%shape('/real1',dims, ierr)
allocate(rr1(dims(1)))
call h5f%read('/real1',rr1, ierr)
if (.not.all(r1 == rr1)) error stop 'real 1-D: read does not match write'

call h5f%shape('/ai1',dims, ierr)
allocate(i1t(dims(1)))
call h5f%read('/ai1',i1t, ierr)
if (.not.all(i1==i1t)) error stop 'integer 1-D: read does not match write'

if (.not. h5f%filename == path//'/test.h5') then
  write(stderr,*) h5f%filename // ' mismatch filename'
  error stop
endif

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine testNewHDF5


subroutine testGroup(path)
character(*), intent(in) :: path

call h5f%initialize(path//'/test_groups.h5', ierr, status='new',action='rw')
call h5f%write('/test/', ierr)
if (ierr /= 0) error stop 'create group'

call h5f%open('/test', ierr)
if (ierr /= 0) error stop 'open group'
call h5f%write('group3/scalar', 1_int32, ierr)
if (ierr /= 0) error stop 'write 0-D: int32'
call h5f%write('group3/scalar64', 1_int64, ierr)
if (ierr /= 0) error stop 'write 0-D: int64'
call h5f%write('group3/scalar_real', 1._real32, ierr)
if (ierr /= 0) error stop 'write 0-D: real32'
call h5f%close(ierr)
if (ierr /= 0) error stop 'close group'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine testGroup


subroutine testwriteHDF5(path)
!! tests that compression doesn't fail for very small datasets, where it really shouldn't be used (makes file bigger)
character(*), intent(in) :: path
integer(int32), dimension(4,4) :: i2, i2t
integer(int64), dimension(4,4) :: i2t64
integer(HSIZE_T), allocatable :: dims(:)
real(real32), allocatable :: rr2(:,:)
real(real32)  ::  nant

i2(1,:) = i1
do i = 1,size(i2,2)
  i2(i,:) = i2(1,:) * i
enddo

r2 = i2

call h5f%initialize(path//'/test.h5', ierr, status='old',action='rw',comp_lvl=1)
if(ierr/=0) error stop 'initialize'
call h5f%write('/test/group2/ai2', i2, ierr)
if(ierr/=0) error stop 'write 2-D: int32'
call h5f%write('/test/group2/ai2_64', int(i2, int64), ierr)
if(ierr/=0) error stop 'write 2-D: int64'
call h5f%write('/test/real2', r2, ierr)
if(ierr/=0) error stop 'write 2-D: real32'
call h5f%write('/nan', nan, ierr)
if(ierr/=0) error stop 'write 0-D: real32 NaN'
call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

call h5f%initialize(path//'/test.h5', ierr,status='old',action='r')

call h5f%read('/test/group2/ai2',i2t, ierr)
if (.not.all(i2==i2t)) error stop 'read 2-D: int32 does not match write'

call h5f%read('/test/group2/ai2_64',i2t64, ierr)
if (.not.all(i2==i2t64)) error stop 'read 2-D: int64 does not match write'

call h5f%shape('/test/real2',dims, ierr)
allocate(rr2(dims(1), dims(2)))
call h5f%read('/test/real2',rr2, ierr)
if (.not.all(r2 == rr2)) error stop 'real 2-D: read does not match write'

call h5f%read('/nan',nant, ierr)
if (.not.ieee_is_nan(nant)) error stop 'failed storing or reading NaN'
call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine testwriteHDF5


subroutine test_hdf5_deflate(path)
character(*), intent(in) :: path
integer(int64), parameter :: N=1000
integer(int64) :: crat
integer ::  fsize, ibig2(N,N) = 0, ibig3(N,N,4) = 0
real(real32) :: big2(N,N) = 0., big3(N,N,4) = 0.
character(:), allocatable :: fn

fn = path//'/test_deflate.h5'

call h5f%initialize(fn, ierr, status='new',action='rw',comp_lvl=1)
call h5f%write('/big2', big2, ierr, chunk_size=[100,100])
call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(big2)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   2D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) write(stderr,*) 'warning: 2D low compression'

!======================================
call h5f%initialize(fn, ierr, status='new',action='rw',comp_lvl=1)
call h5f%write('/big3', big3, ierr, chunk_size=[100,100,1])
call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(big3)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) write(stderr,*) 'warning: 3D low compression'
!======================================
call h5f%initialize(fn, ierr, status='new',action='rw',comp_lvl=1)
call h5f%write('/ibig3', ibig3, ierr, chunk_size=[1000,100,1])
call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(ibig3)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) write(stderr,*) 'warning: 3D low compression'
!======================================
call h5f%initialize(fn, ierr, status='new',action='rw',comp_lvl=1)

call h5f%write('/ibig2', ibig2, ierr, chunk_size=[100,100])

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(ibig2)/8) / fsize

print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) write(stderr,*) 'warning: 3D low compression'

end subroutine test_hdf5_deflate


subroutine test_write_attributes(path)
character(*), intent(in) :: path

call h5f%initialize(path//'/test_deflate.h5', ierr)

call h5f%writeattr('/little/','note','this is just a little number', ierr)
if (ierr /= 0) error stop 'write attribute string'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine test_write_attributes


subroutine test_string_rw(path)
character(*), intent(in) :: path
character(2) :: value
character(1024) :: val1k
character(:), allocatable :: final
integer :: i

call h5f%initialize(path//'/test_string.h5', ierr, status='new', action='rw')
call h5f%write('/little', '42', ierr)

call h5f%read('/little', value, ierr)

if (value /= '42') error stop 'string dataset read/write verification failure. Value: '// value

!! try reading too much data, then truncating to first C_NULL
call h5f%read('/little', val1k, ierr)
final = truncate_string_null(val1k)

if (len(final) /= 2) then
  write(stderr, *) 'trimming str to c_null did not work, got len() = ', len(final)
  write(stderr, *) iachar(final(3:3))
  error stop
endif

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

end subroutine test_string_rw


subroutine testrwHDF5(path,ng, nn, pn)
!! more group
character(*), intent(in) :: path
integer, intent(in) :: ng, nn, pn

real(real32), allocatable :: flux(:,:),fo(:)
character(2) :: pnc,ic
integer :: i

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

end subroutine testrwHDF5


subroutine test_writeExistingVariable(path)
character(*), intent(in) :: path
integer :: ierr
character(:),allocatable :: fn

fn = path//'/overwrite.h5'

call h5f%initialize(fn, ierr, status='new',action='w')
if (ierr /= 0) error stop 'write initialize'

call h5f%write('/scalar_int', 42_int32, ierr)
if (ierr /= 0) error stop 'write scalar int'

call h5f%write('/int1d', [42_int32, 1_int32], ierr)
if (ierr /= 0) error stop 'write 1D int'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'write finalize'

call h5f%initialize(fn, ierr, status='old',action='rw')
if (ierr /= 0) error stop 'overwrite initialize'

call h5f%write('/scalar_int', 100_int32, ierr)
if (ierr /= 0) error stop 'overwrite scalar int'

call h5f%write('/int1d', [100_int32, 10_int32], ierr)
if (ierr /= 0) error stop 'overwrite 1D int'

call h5f%finalize(ierr)
if (ierr /= 0) error stop 'overwrite finalize'


end subroutine test_writeExistingVariable

end program
