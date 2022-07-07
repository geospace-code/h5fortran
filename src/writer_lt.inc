type(hdf5_file) :: h

call h%open(filename, action='rw')
call h%write(dname, A)
call h%close()
