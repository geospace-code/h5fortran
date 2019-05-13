submodule (hdf5_interface:write) write_int


contains


module procedure hdf_add_int

integer(HID_T) :: sid,did
integer         :: ierr

call self%add(dname)

!> HDF5 >= 1.10
!call h5ltmake_dataset_f(self%lid, dname, &
!  rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_INTEGER_KIND), value, ierr)
!if (ierr /= 0) error stop 'error on dataset '//dname//' write '//self%filename

!>  HDF5 1.8 compatbility below:
!> create dataspace
call h5screate_f(H5S_SCALAR_F, sid, ierr)
if (ierr /= 0) error stop 'error create dataspace ' //dname// ' write ' //self%filename

!> create dataset
call h5dcreate_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), sid, did, ierr)
if (ierr /= 0) error stop 'error create dataet ' //dname// ' write ' //self%filename

!> write dataset
call h5dwrite_f(did, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, int(shape(value),HSIZE_T), ierr)
if (ierr /= 0) error stop 'error write dataspace ' //dname// ' write ' //self%filename

!> close space and dataset
call h5dclose_f(did, ierr)
if (ierr /= 0) error stop 'error close dataset ' //dname// ' write ' //self%filename
call h5sclose_f(sid, ierr)
if (ierr /= 0) error stop 'error close dataspace ' //dname// ' write ' //self%filename

end procedure hdf_add_int


module procedure hdf_add_int_1d

integer         :: ierr

call self%add(dname)

call h5ltmake_dataset_f(self%lid, dname, &
  rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_INTEGER_KIND), value, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' write ' //self%filename

end procedure hdf_add_int_1d


module procedure hdf_add_int_2d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' write ' //self%filename

call hdf_wrapup(self)

end procedure hdf_add_int_2d


module procedure hdf_add_int_3d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' write ' //self%filename

call hdf_wrapup(self)

end procedure hdf_add_int_3d


module procedure hdf_add_int_4d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' write ' //self%filename

call hdf_wrapup(self)

end procedure hdf_add_int_4d


module procedure hdf_add_int_5d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' write ' //self%filename

call hdf_wrapup(self)

end procedure hdf_add_int_5d


module procedure hdf_add_int_6d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' write ' //self%filename

call hdf_wrapup(self)

end procedure hdf_add_int_6d


module procedure hdf_add_int_7d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' write ' //self%filename

call hdf_wrapup(self)

end procedure hdf_add_int_7d


end submodule write_int
