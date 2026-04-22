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

  ! 1. Writer side
  call h_write%open(fn, action='w', swmr=.true.)
  call h_write%write('/data', dat)
  call h_write%flush('/data')

  ! 2. Reader side (concurrent-ish)
  call h_read%open(fn, action='r', swmr=.true.)
  call h_read%read('/data', dat_out)

  if (any(dat_out /= dat)) then
     print *, 'ERROR: SWMR data mismatch'
     stop 1
  end if

  ! 3. Update data
  dat = dat * 2.0
  call h_write%write('/data', dat)
  call h_write%flush('/data')

  ! 4. Refresh reader
  call h_read%refresh('/data')
  call h_read%read('/data', dat_out)

  if (any(dat_out /= dat)) then
     print *, 'ERROR: SWMR data mismatch after update'
     print *, 'Expected:', dat
     print *, 'Got:     ', dat_out
     stop 1
  end if

  call h_read%close()
  call h_write%close()

  print *, 'PASSED: SWMR'

end program test_swmr
