!! This submodule is for reading integer HDF5 data
submodule (hdf5_interface:read) read_int

use H5LT, only: HSIZE_T, SIZE_T, H5_INTEGER_KIND, H5KIND_TO_TYPE, h5ltpath_valid_f
implicit none
contains


module procedure hdf_get_int

integer(HID_T) :: dsid
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (.not.exists) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

!> open dataset
call h5dopen_f(self%lid, dname, dsid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: open ' // dname // ' read ' // self%filename
  return
endif

!> read dataset
call h5dread_f(dsid, h5kind_to_type(kind(value), H5_INTEGER_KIND), value,int(shape(value),HSIZE_T), ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

!> close dataset
call h5dclose_f(dsid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: close ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_get_int


module procedure hdf_get_int_1d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: dtype
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (.not.exists) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: open ' // dname // ' read ' // self%filename
  return
endif

allocate(value(dims(1)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_get_int_1d


module procedure hdf_get_int_2d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: dtype
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (.not.exists) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: open ' // dname // ' read ' // self%filename
  return
endif

allocate(value(dims(1), dims(2)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_get_int_2d


module procedure hdf_get_int_3d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: dtype
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (.not.exists) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: open ' // dname // ' read ' // self%filename
  return
endif

allocate(value(dims(1), dims(2), dims(3)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_get_int_3d


module procedure hdf_get_int_4d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: dtype
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (.not.exists) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: open ' // dname // ' read ' // self%filename
  return
endif

allocate(value(dims(1), dims(2), dims(3), dims(4)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_get_int_4d


module procedure hdf_get_int_5d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: dtype
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (.not.exists) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: open ' // dname // ' read ' // self%filename
  return
endif

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_get_int_5d


module procedure hdf_get_int_6d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: dtype
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (.not.exists) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: open ' // dname // ' read ' // self%filename
  return
endif

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_get_int_6d


module procedure hdf_get_int_7d

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer :: dtype
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (.not.exists) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: open ' // dname // ' read ' // self%filename
  return
endif

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_get_int_7d


end submodule read_int
