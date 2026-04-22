program test_swmr
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'test_swmr.h5'
  type(hdf5_file) :: h
  integer, parameter :: n = 10
  real :: dat(n), dat_out(n)
  integer :: i

  do i = 1, n
    dat(i) = real(i)
  end do

  ! Write with swmr mode
  call h%open(fn, action='w', swmr=.true.)
  call h%write('/data', dat)
  call h%flush('/data')
  call h%close()

  ! Read (without swmr flag)
  call h%open(fn, action='r')
  call h%read('/data', dat_out)
  call h%close()

  if (any(dat_out /= dat)) then
    print '(A)', 'ERROR: data mismatch'
    call exit(1)
  end if

  print '(A)', 'PASSED'

end program test_swmr