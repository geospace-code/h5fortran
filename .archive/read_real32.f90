!! This submodule is for reading single precision float (32 bit)
submodule (hdf5_interface:read) read_real32

use H5LT, only: HSIZE_T, SIZE_T, H5_REAL_KIND, H5KIND_TO_TYPE, h5ltpath_valid_f
implicit none
contains


module procedure hdf_read_real32

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_real32


module procedure hdf_read_real32_1d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_real32_1d


module procedure hdf_read_real32_2d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1), dims(2)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_real32_2d


module procedure hdf_read_real32_3d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1), dims(2), dims(3)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_real32_3d


module procedure hdf_read_real32_4d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1), dims(2), dims(3), dims(4)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_real32_4d


module procedure hdf_read_real32_5d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_real32_5d


module procedure hdf_read_real32_6d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_real32_6d


module procedure hdf_read_real32_7d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_real32_7d


end submodule read_real32
