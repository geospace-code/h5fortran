type(hdf5_file) :: h

call h%open(filename, action='r')
call h%read(dname, A)
call h%close()
