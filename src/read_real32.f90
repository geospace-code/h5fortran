submodule (hdf5_interface:read) read_real32


contains


module procedure hdf_get_real32

integer(HID_T)  :: dsid
integer :: ierr

!> open dataset
call h5dopen_f(self%lid, dname, dsid, ierr)
if (ierr /= 0) error stop 'error open dataset ' //dname// ' read ' //self%filename

!> read dataset
call h5dread_f(dsid, h5kind_to_type(kind(value),H5_REAL_KIND), value,int(shape(value),HSIZE_T), ierr)
if (ierr /= 0) error stop 'error read dataset ' //dname// ' read ' //self%filename

!> close dataset
call h5dclose_f(dsid, ierr)
if (ierr /= 0) error stop 'error close dataset ' //dname// ' read ' //self%filename

end procedure hdf_get_real32


module procedure hdf_get_real32_1d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset ' //dname// ' read ' //self%filename

allocate(value(dims(1)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset ' //dname// ' read ' //self%filename

end procedure hdf_get_real32_1d


module procedure hdf_get_real32_2d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset ' //dname// ' read ' //self%filename

allocate(value(dims(1),dims(2)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset ' //dname// ' read ' //self%filename

end procedure hdf_get_real32_2d


module procedure hdf_get_real32_3d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset ' //dname// ' read ' //self%filename

allocate(value(dims(1),dims(2),dims(3)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset ' //dname// ' read ' //self%filename

end procedure hdf_get_real32_3d


module procedure hdf_get_real32_4d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset ' //dname// ' read ' //self%filename

allocate(value(dims(1),dims(2),dims(3),dims(4)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset ' //dname// ' read ' //self%filename

end procedure hdf_get_real32_4d


module procedure hdf_get_real32_5d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset ' //dname// ' read ' //self%filename

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset ' //dname// ' read ' //self%filename

end procedure hdf_get_real32_5d


module procedure hdf_get_real32_6d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset ' //dname// ' read ' //self%filename

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset ' //dname// ' read ' //self%filename

end procedure hdf_get_real32_6d


module procedure hdf_get_real32_7d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: ierr, dtype

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error open dataset ' //dname// ' read ' //self%filename

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) error stop 'error read dataset ' //dname// ' read ' //self%filename

end procedure hdf_get_real32_7d


end submodule read_real32
