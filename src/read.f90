submodule (hdf5_interface) read


contains


module procedure hdf_get_string
! FIXME need to use another function to read length, as simply reads gibberish after actual data
! till buffer is full

integer :: ierr

call h5ltread_dataset_string_f(self%lid, dname, value, ierr)
if (ierr /= 0) error stop 'error on dataset '//dname//' read '//self%filename

end procedure hdf_get_string


end submodule read
