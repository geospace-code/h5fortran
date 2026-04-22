program test_swmr_reader
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'swmr.h5'
  type(hdf5_file) :: h
  integer, parameter :: n = 10, niter = 3
  real :: dat(n)
  integer :: k

  call h%open(fn, action='r', swmr=.true.)

  k = 0
  do
    k = k + 1
    call h%refresh('/data')
    call h%read('/data', dat)
    print '(A,I0,A,F5.2)', 'READER:', k, ' v=', dat(1)
    if (k >= niter) exit
    call execute_command_line('sleep 0.3')
  end do
  call h%close()
  print '(A)', 'READER: done'

end program test_swmr_reader