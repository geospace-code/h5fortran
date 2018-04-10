program test_hdf5_ifc

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  use, intrinsic:: iso_fortran_env, only: int64, real32, real64
  use hdf5_interface, only: hdf5_file, toLower

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
  call testNewHDF5()
  call testAddHDF5()
  call test_hdf5_deflate()
  call test_write_attributes()
  call test_string_rw()
  call testrwHDF5(ng=69, nn=100, pn=5)

contains

  subroutine test_lowercase()

    character(*), parameter :: hello = 'HeLl0 Th3rE !>? '
    character(:), allocatable :: hello_alloc   ! Fortran 2003


    if (.not.(toLower(hello)=='hell0 th3re !>? ')) error stop 'error: lowercase conversion'

    ! Fortran 2003 allocatable string
    hello_alloc = trim(toLower(hello))
    if (.not.(hello_alloc=='hell0 th3re !>?')) error stop 'Allocatable lowercase conversion error'
    
    
    if(.not.all(toLower(['Hi','hI'])==['hi','hi'])) error stop 'error on array conversion'

  end subroutine test_lowercase


  subroutine testNewHDF5()
  
    real(real32), allocatable :: rr1(:)
    real(real32) :: rt
    integer, allocatable :: i1t(:)

    call h5f%initialize('test.h5',status='new',action='w')

    call h5f%add('/test/scalar_int',42)
    call h5f%add('/test/scalar_real',42.)
    call h5f%get('/test/scalar_real',rt)
    if (.not.rt==42.) error stop 'real scalar not equal'
    
    call h5f%add('/test/real1',r1)
    call h5f%get('/test/real1',rr1)
    if (.not.all(r1 == rr1)) error stop 'real: read does not match write'
    
    
    call h5f%add('/test/ai1', i1)
    call h5f%get('/test/ai1',i1t)

    call h5f%finalize()

    if (.not.all(i1==i1t)) error stop 'integer: read does not match write'
    
    
    print *, h5f%filename
    
    if (.not. h5f%filename == 'test.h5') error stop h5f%filename//' mismatch filename'
    
 end subroutine testNewHDF5


subroutine testAddHDF5()

  integer :: i2(4,4)
  integer, allocatable :: i2t(:,:)
  real(real32), allocatable :: rr2(:,:)
  integer :: one = 1
  real(real32)   :: ro = 1.0, nant

  i2(1,:) = i1
  do concurrent (i = 1:size(i2,2))
    i2(i,:) = i2(1,:) * i
  enddo
  
  r2 = i2

! tests that compression doesn't fail for very small datasets, where it really shouldn't be used (makes file bigger)
  call h5f%initialize('test.h5',status='old',action='rw',comp_lvl=1)

  call h5f%open('/test/')
  call h5f%add('group3/scalar',one)
  call h5f%add('group3/scalar_real',ro)
  call h5f%close()

  call h5f%add('/test/group2/ai2', i2)
  call h5f%get('/test/group2/ai2',i2t)
  if (.not.all(i2==i2t)) error stop 'read does not match write'
  
  call h5f%add('/test/real2',r2)
  call h5f%get('/test/real2',rr2)
  if (.not.all(r2 == rr2)) error stop 'real: read does not match write'

  call h5f%add('/test/nan',nan)
  call h5f%get('/test/nan',nant)
  if (.not.ieee_is_nan(nant)) error stop 'not reading NaN'

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
  
  if (h5f%comp_lvl > 0 .and. crat < 10) print *,'warning: 2D low compression' 
  
!======================================
  call h5f%initialize('test_deflate.h5',status='new',action='rw',comp_lvl=1)

  call h5f%add('/big3', big3, chunk_size=[100,100,1])
  
  call h5f%finalize()
  
  inquire(file='test_deflate.h5', size=fsize)
  crat = (N*N*storage_size(big3)/8) / fsize
  
  print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat
  
  if (h5f%comp_lvl > 0 .and. crat < 10) print *,'warning: 3D low compression' 
!======================================
  call h5f%initialize('test_deflate.h5',status='new',action='rw',comp_lvl=1)

  call h5f%add('/ibig3', ibig3, chunk_size=[1000,100,1])
  
  call h5f%finalize()
  
  inquire(file='test_deflate.h5', size=fsize)
  crat = (N*N*storage_size(ibig3)/8) / fsize
  
  print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat
  
  if (h5f%comp_lvl > 0 .and. crat < 10) print *,'warning: 3D low compression' 
!======================================
  call h5f%initialize('test_deflate.h5',status='new',action='rw',comp_lvl=1)

  call h5f%add('/ibig2', ibig2, chunk_size=[100,100])
  
  call h5f%finalize()
  
  inquire(file='test_deflate.h5', size=fsize)
  crat = (N*N*storage_size(ibig2)/8) / fsize
  
  print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat
  
  if (h5f%comp_lvl > 0 .and. crat < 10) print *,'warning: 3D low compression' 


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
    
    if (.not.value=='42') error stop 'string dataset read/write verification failure'

    call h5f%finalize()
    
end subroutine test_string_rw


subroutine testrwHDF5(ng, nn, pn)
  !> more group

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

end program test_hdf5_ifc
