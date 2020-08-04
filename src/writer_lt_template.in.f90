type(hdf5_file) :: h
integer :: ier

call h%initialize(filename, ier, status='unknown')
if (ier == 0) call h%write(dname, value, ier)
if (ier == 0) call h%finalize(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname)) then
  if (present(ierr)) return
  error stop
endif
