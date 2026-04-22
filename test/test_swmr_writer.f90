program test_swmr_writer
  use h5fortran, only: hdf5_file
  implicit none

  character(*), parameter :: fn = 'swmr.h5'
  type(hdf5_file) :: h
  integer, parameter :: n = 10, niter = 5
  real :: dat(n)
  integer :: i, k, c1, cr
  logical :: exists

  call system_clock(c1, cr)

  call h%open(fn, action='w', swmr=.true.)
  do i = 1, n
    dat(i) = real(i)
  end do
  call h%write('/data', dat)
  call h%flush('/data')
  call h%close()
  call system_clock(cr)
  print '(A,F6.3)', 'WRITER: created t=', real(cr-c1)/1000

  ! Wait for reader to be ready (signal file)
  do
    inquire(file='reader_ready', exist=exists)
    if (exists) exit
    call execute_command_line('sleep 0.1')
  end do

  ! Now write updates
  call h%open(fn, action='r+', swmr=.true.)
  do k = 1, niter
    do i = 1, n
      dat(i) = real(k) + real(i-1)
    end do
    call h%write('/data', dat)
    call h%flush('/data')
    call system_clock(cr)
    print '(A,I0,A,F6.3)', 'WRITER:', k, ' t=', real(cr-c1)/1000
    call execute_command_line('sleep 0.5')
  end do
  call h%close()
  print '(A,F6.3)', 'WRITER: done'

end program test_swmr_writer