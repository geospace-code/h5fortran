submodule (hdf5_interface:read) read_int


contains


module procedure hdf_get_int

integer(HID_T)  :: dsid
integer :: ierr

!> open dataset
call h5dopen_f(self%lid, dname, dsid, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

! read dataset
call h5dread_f(dsid, h5kind_to_type(kind(value),H5_INTEGER_KIND), value,int(shape(value),HSIZE_T), ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

!> close dataset
call h5dclose_f(dsid, ierr)
if (ierr /= 0) error stop 'error close dataset '//dname//' read '//self%filename

end procedure hdf_get_int


module procedure hdf_get_int_1d

integer(HSIZE_T) :: dims(1)
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_int_1d


module procedure hdf_get_int_2d

integer(HSIZE_T) :: dims(2)
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1),dims(2)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_int_2d


module procedure hdf_get_int_3d

integer(HSIZE_T) :: dims(3)
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1),dims(2),dims(3)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_int_3d


end submodule read_int
