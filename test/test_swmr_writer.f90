program test_swmr_writer
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'swmr.h5'
  type(hdf5_file) :: h
  integer, parameter :: n = 10, niter = 3
  real :: dat(n)
  integer :: i, k

  call h%open(fn, action='w', swmr=.true.)
  do i = 1, n
    dat(i) = real(i)
  end do
  call h%write('/data', dat)
  call h%flush('/data')
  call h%close()
  print '(A)', 'WRITER: created'

  call execute_command_line('sleep 1')

  call h%open(fn, action='r+', swmr=.true.)
  do k = 1, niter
    do i = 1, n
      dat(i) = real(k) + real(i-1)
    end do
    call h%write('/data', dat)
    call h%flush('/data')
    print '(A,I0,A)', 'WRITER:', k, ' wrote'
    call execute_command_line('sleep 0.5')
  end do
  call h%close()
  print '(A)', 'WRITER: done'

end program test_swmr_writer