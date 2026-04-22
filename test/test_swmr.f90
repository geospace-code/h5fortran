program test_swmr
  implicit none
  integer :: stat

  call execute_command_line('rm -f swmr.h5')

  call execute_command_line('./test_swmr_writer & wait $!; ./test_swmr_reader', exitstat=stat)

  if (stat == 0) then
    print '(A)', 'PASSED'
  else
    print '(A)', 'FAILED'
    call exit(1)
  end if

end program test_swmr