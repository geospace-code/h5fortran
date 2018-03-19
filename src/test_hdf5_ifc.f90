program test_hdf5_ifc

  use, intrinsic:: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan
  use hdf5_interface, only: hdf5_file, toLower

  implicit none

  type(hdf5_file) :: h5f
  integer :: i1(4)
  integer :: one = 1
  real    :: ro = 1.0
  real    :: nan,nant
  integer, parameter :: realbits = storage_size(ro)

  integer, allocatable :: i1t(:),i2t(:,:)

  integer :: i
  character(2) :: ic, cs

  nan = ieee_value(1.0, ieee_quiet_nan)

  write(cs,'(I2)') realbits

  do concurrent (i = 1:size(i1))
    i1(i) = i
  enddo

  print *,'real kind bits: ',cs

  call test_lowercase()
  call testNewHDF5()
  call testAddHDF5()
  call testrwHDF5(ng=69, nn=100, pn=5)

  contains

  subroutine test_lowercase()

    character(*), parameter :: hello = 'HeLl0 Th3rE !>? '
    character(:), allocatable :: hello_alloc   ! Fortran 2003


    if (.not.(toLower(hello)=='hell0 th3re !>? ')) error stop 'error: lowercase conversion'

    ! Fortran 2003 allocatable string
    hello_alloc = trim(toLower(hello))
    if (.not.(hello_alloc=='hell0 th3re !>?')) error stop 'Allocatable lowercase conversion error'

  end subroutine test_lowercase

  subroutine testNewHDF5()

    call h5f%initialize('test_'//cs//'.h5',status='new',action='w')


    call h5f%add('/test/ai1', i1)
    call h5f%get('/test/ai1',i1t)

    call h5f%finalize()

    if (.not.all(i1==i1t)) error stop 'read does not match write'

 end subroutine testNewHDF5

 subroutine testAddHDF5()

  integer :: i2(4,4)

  i2(1,:) = i1
  do concurrent (i = 1:size(i2,2))
    i2(i,:) = i2(1,:) * i
  enddo

  call h5f%initialize('test_'//cs//'.h5',status='old',action='rw')

  call h5f%open('/test/')
  call h5f%add('group3/scalar',one)
  call h5f%add('group3/scalar_real',ro)
  call h5f%close()

  call h5f%add('/test/group2/ai2', i2)
  call h5f%get('/test/group2/ai2',i2t)
  if (.not.all(i2==i2t)) error stop 'read does not match write'

  call h5f%add('/test/nan',nan)
  call h5f%get('/test/nan',nant)
  if (.not.ieee_is_nan(nant)) error stop 'not reading NaN'

  call h5f%finalize()

end subroutine testAddHDF5

subroutine testrwHDF5(ng, nn, pn)
  !> more group

  integer, intent(in) :: ng, nn, pn

  real, allocatable :: flux(:,:),fo(:)
  character(2) :: pnc
  integer :: i

  allocate(flux(nn,ng),fo(nn))
  flux = ro
  write(pnc,'(I2)') pn

  call h5f%initialize('p'//trim(adjustl(pnc))//'_'//cs//'.h5',status='new',action='w')

  do i = 1,ng
    write(ic,'(I2)') i
    call h5f%add('/group'//trim(adjustl(ic))//'/flux_node',flux(:ng,i))
  enddo



  call h5f%get('/group1/flux_node',fo)
  if (.not.all(fo(:ng)==flux(:ng,1))) error stop 'test_read_write: read does not match write'

  call h5f%finalize()

end subroutine testrwHDF5

end program test_hdf5_ifc
