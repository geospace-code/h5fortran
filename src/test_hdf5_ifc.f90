program test_hdf5_ifc
  use hdf5_interface
  implicit none

  type(hdf5_file) :: h5f
  integer :: i1(4),i2(4,4)
  integer :: one = 1
  real    :: ro = 1.0
  integer, allocatable :: i1t(:),i2t(:,:)
  integer :: i

  do i = 1,size(i1)
    i1(i) = i
    i2(i,:) = i
  enddo

  print*, 'initialize ...'
  call h5f%initialize('test.h5',status='NEW',action='WRITE')


  print*, 'add dataset ...'
  call h5f%add('/test/ai1', i1)
  print*,i1

  print*, 'get dataset ...'
  call h5f%get('/test/ai1',i1t)
  print*,i1t

  call h5f%open('/test/')
  call h5f%add('group3/scalar',one)
    call h5f%add('group3/scalar_real',ro)
  call h5f%close()

  print*, 'add dataset ...'
  call h5f%add('/test/group2/ai2', i2)
  print*,i2

  print*, 'get dataset ...'
  call h5f%get('/test/group2/ai2',i2t)
  print*,i2t


  call h5f%finalize()

end program test_hdf5_ifc
