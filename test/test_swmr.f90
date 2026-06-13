program test_swmr
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'test_swmr.h5'
  type(hdf5_file) :: h_write, h_read
  integer, parameter :: n = 10
  real :: dat(n), dat_out(n)
  integer :: i

  do i = 1, n
    dat(i) = real(i)
  end do

  ! Writer: create with swmr mode
  call h_write%open(fn, action='w', swmr=.true.)
  call h_write%write('/data', dat)
  call h_write%flush('/data')

  ! Writer: update data while still open
  dat = dat * 2.0
  call h_write%write('/data', dat)
  call h_write%flush('/data')
  call h_write%close()

  ! Reader: open file created with swmr mode
  call h_read%open(fn, action='r')
  call h_read%refresh('/data')
  call h_read%read('/data', dat_out)
  call h_read%close()

  if (any(dat_out /= dat)) then
    print '(A)', 'ERROR: data mismatch'
    print '(A,10F5.2)', 'Expected:', dat
    print '(A,10F5.2)', 'Got:     ', dat_out
    call exit(1)
  end if

  print '(A)', 'PASSED: SWMR write/update/read'

end program test_swmr