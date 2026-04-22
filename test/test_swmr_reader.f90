program test_swmr_reader
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'swmr.h5'
  type(hdf5_file) :: h
  integer, parameter :: n = 10
  real :: dat(n)
  integer :: k, c1, cr
  logical :: ok

  call system_clock(c1, cr)

  ! Try to open, exit if file not ready
  call h%open(fn, action='r', swmr=.true., ok=ok)
  if (.not. ok) then
    print *, 'READER: file not ready'
    call exit(1)
  end if

  k = 0
  do
    k = k + 1
    call h%refresh('/data')
    call h%read('/data', dat)
    call system_clock(cr)
    print '(A,I0,A,F6.3,A,F5.2)', 'READER:', k, ' t=', real(cr-c1)/1000, ' v=', dat(1)
    if (k >= 5) exit
  end do
  call h%close()

end program test_swmr_reader