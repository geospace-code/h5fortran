program test_swmr_reader
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'swmr.h5'
  type(hdf5_file) :: h
  integer, parameter :: n = 10
  real :: dat(n), last_val
  integer :: k, c1, cr
  logical :: ok

  call system_clock(c1, cr)
  last_val = -1

  ! Wait for file to exist
  do
    inquire(file=fn, exist=ok)
    if (ok) exit
    call execute_command_line('sleep 0.1')
  end do

  k = 0
  do
    k = k + 1
    call h%open(fn, action='r', ok=ok)
    if (ok) then
      call h%refresh('/data')
      call h%read('/data', dat)
      call h%close()
      call system_clock(cr)
      if (dat(1) /= last_val) then
        last_val = dat(1)
        print '(A,I0,A,F6.3,A,F5.2)', 'READER:', k, ' t=', real(cr-c1)/1000, ' v=', dat(1)
      end if
    end if
    if (k >= 10) exit
    call execute_command_line('sleep 0.5')
  end do
  if (ok) then
    call system_clock(cr)
    print '(A,F6.3)', 'READER: done t=', real(cr-c1)/1000
  else
    print '(A)', 'READER: never succeeded'
  end if

end program test_swmr_reader