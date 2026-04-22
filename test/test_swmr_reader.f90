program test_swmr_reader
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'swmr.h5'
  type(hdf5_file) :: h
  integer, parameter :: n = 10
  real :: dat(n)
  integer :: k

  call h%open(fn, action='r', swmr=.true.)
  k = 0
  do
    k = k + 1
    call h%refresh('/data')
    call h%read('/data', dat)
    print *, 'READER:', k, 'got', dat(1), '...', dat(n)
    if (k >= 5) exit
  end do
  call h%close()

end program test_swmr_reader