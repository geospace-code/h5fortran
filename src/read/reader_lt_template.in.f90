type(hdf5_file) :: h
integer :: ier

call h%open(filename, ier, status='old', action='r')
if (ier == 0) call h%read(dname, value, ier)
if (ier == 0) call h%close(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
