!! This submodule is for reading integer HDF5 data
submodule (hdf5_interface:read) read_int

use H5LT, only: HSIZE_T, SIZE_T, H5_INTEGER_KIND, H5KIND_TO_TYPE, h5ltpath_valid_f
implicit none
contains


module procedure hdf_read_int

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_int


module procedure hdf_read_int_1d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_int_1d


module procedure hdf_read_int_2d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1), dims(2)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_int_2d


module procedure hdf_read_int_3d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1), dims(2), dims(3)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_int_3d


module procedure hdf_read_int_4d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1), dims(2), dims(3), dims(4)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_int_4d


module procedure hdf_read_int_5d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_int_5d


module procedure hdf_read_int_6d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_int_6d


module procedure hdf_read_int_7d

integer(HSIZE_T) :: dims(rank(value))

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

allocate(value(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))

call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, dims,  ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename
  return
endif

end procedure hdf_read_int_7d


end submodule read_int
