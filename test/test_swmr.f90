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

  ! 1. Writer side
  call h%open(fn, action='w', swmr=.true.)
  call h%write('/data', dat)
  call h%flush('/data')

  print *, 'Writer opened with swmr=', h%swmr

  call h%close()

  ! 2. Reader side
  call h%open(fn, action='r', swmr=.true.)
  print *, 'Reader opened with swmr=', h%swmr
  call h%read('/data', dat_out)

  if (any(dat_out /= dat)) then
     print *, 'ERROR: SWMR data mismatch'
     print *, 'Expected:', dat
     print *, 'Got:     ', dat_out
     call h%close()
     stop 1
  end if

  ! 3. Update data as writer
  dat = dat * 2.0
  call h%close()

  call h%open(fn, action='r+', swmr=.true.)
  call h%write('/data', dat)
  call h%flush('/data')
  call h%close()

  ! 4. Refresh reader
  call h%open(fn, action='r', swmr=.true.)
  call h%refresh('/data')
  call h%read('/data', dat_out)

  if (any(dat_out /= dat)) then
     print *, 'ERROR: SWMR data mismatch after update'
     print *, 'Expected:', dat
     print *, 'Got:     ', dat_out
     call h%close()
     stop 1
  end if

  call h%close()
  print *, 'PASSED: SWMR'
  print *, 'NOTE: Uses latest libver mode for SWMR compatibility'

end program test_swmr