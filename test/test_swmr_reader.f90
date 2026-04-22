program test_swmr_reader
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'swmr.h5'
  type(hdf5_file) :: h
  integer, parameter :: n = 10, niter = 5
  real :: dat(n)
  integer :: k, c1, cr

  call system_clock(c1, cr)

  call h%open(fn, action='r', swmr=.true.)
  call h%close()

  ! Signal writer that reader is ready
  open(10, file='reader_ready')
  close(10, status='delete')

  k = 0
  do
    k = k + 1
    call h%open(fn, action='r', swmr=.true.)
    call h%refresh('/data')
    call h%read('/data', dat)
    call h%close()
    call system_clock(cr)
    print '(A,I0,A,F6.3,A,F5.2)', 'READER:', k, ' t=', real(cr-c1)/1000, ' v=', dat(1)
    if (k >= niter) exit
    call execute_command_line('sleep 0.3')
  end do
  print '(A,F6.3)', 'READER: done'

end program test_swmr_reader