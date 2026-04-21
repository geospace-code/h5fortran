type(hdf5_file) :: h
logical :: ok
integer :: ier

ier = -1

call h%open(filename, action='r', ok=ok)
if (ok) call h%read(dname, A, ierr=ier)
call h%close()

if(present(ierr)) then
  ierr = ier
elseif(ier /= 0 .or. .not. ok) then
  error stop "ERROR:h5fortran:h5read dataset " // dname // " file: " // filename
endif
