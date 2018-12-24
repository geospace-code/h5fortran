submodule (hdf5_interface:read) read_real64


contains


module procedure hdf_get_real64

integer(HID_T)  :: set_id
integer :: ierr

!> open dataset
call h5dopen_f(self%lid, dname, set_id, ierr)

!> read dataset
call h5dread_f(set_id, h5kind_to_type(kind(value),H5_REAL_KIND), value,int(shape(value),HSIZE_T), ierr)

!> close dataset
call h5dclose_f(set_id, ierr)

end procedure hdf_get_real64


module procedure hdf_get_real64_1d

integer(SIZE_T) :: dims(1),dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_real64_1d


module procedure hdf_get_real64_2d

integer(SIZE_T) :: dims(2),dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1),dims(2)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_real64_2d


module procedure hdf_get_real64_3d

integer(SIZE_T) :: dims(3),dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1),dims(2),dims(3)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_real64_3d


module procedure hdf_get_real64_4d

integer(SIZE_T) :: dims(4),dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1),dims(2),dims(3),dims(4)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_real64_4d


module procedure hdf_get_real64_5d

integer(SIZE_T) :: dims(5),dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_real64_5d


module procedure hdf_get_real64_6d

integer(SIZE_T) :: dims(6),dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset '//dname//' read '//self%filename

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset '//dname//' read '//self%filename

end procedure hdf_get_real64_6d


end submodule read_real64
