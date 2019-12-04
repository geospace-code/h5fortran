!! This submodule is for writing HDF5 data via child submodules
submodule (hdf5_interface) write

use H5LT, only: h5screate_f, h5sclose_f, h5dcreate_f, h5dwrite_f, h5dclose_f, h5ltmake_dataset_f, &
  h5ltpath_valid_f, h5ltset_attribute_string_f, h5screate_simple_f, &
  h5pset_chunk_f, h5pset_deflate_f, h5pset_shuffle_f, h5pcreate_f, H5P_DATASET_CREATE_F, h5pclose_f, &
  h5ltmake_dataset_string_f, h5gopen_f, h5gclose_f
implicit none
contains


module procedure writeattr

integer :: ierr
logical :: exists

call self%add(dname)

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (ierr /= 0) error stop 'problem checking existence: ' //dname// ' file ' //self%filename

if (.not.exists) then
  write(stderr,*) 'WARNING: variable ' //dname// ' must be created before writing ' //attr
  return
endif

call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
if (ierr /= 0) error stop 'problem writing attribute ' //attr// ' to ' //dname// ' file ' //self%filename

end procedure writeattr


module procedure hdf_add_string
!! subroutine hdf_add_string(self, dname, value)
integer :: ierr

call h5ltmake_dataset_string_f(self%lid, dname, value, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' write ' //self%filename

end procedure hdf_add_string


module procedure hdf_setup_write

integer :: ierr

call self%add(dname)

if (present(chunk_size)) self%chunk_size(:size(dims)) = chunk_size

call hdf_set_deflate(self, dims)

call h5screate_simple_f(size(dims), dims, self%sid, ierr)
if (ierr /= 0) error stop 'error on dataspace ' //dname// ' ' //self%filename

call h5dcreate_f(self%lid, dname, dtype, self%sid, self%did, ierr, self%pid)
if (ierr /= 0) error stop 'error on dataset ' //dname// ' ' //self%filename

end procedure hdf_setup_write


module procedure hdf_set_deflate

integer :: ierr, ndims, i
integer(HSIZE_T), allocatable :: chunk_size(:)


ndims = size(dims)
allocate(chunk_size(ndims))

do concurrent (i=1:ndims)
  chunk_size(i) = min(self%chunk_size(i), dims(i))
enddo

if (self%verbose) print *,'dims: ',dims,'chunk size: ',chunk_size

call h5pcreate_f(H5P_DATASET_CREATE_F, self%pid, ierr)
if (ierr /= 0) error stop 'error creating property '//self%filename

call h5pset_chunk_f(self%pid, ndims, chunk_size, ierr)
if (ierr /= 0) error stop 'error setting chunk '//self%filename

if (self%comp_lvl < 1 .or. self%comp_lvl > 9) return


call h5pset_shuffle_f(self%pid, ierr)
if (ierr /= 0) error stop 'error enabling Shuffle '//self%filename

call h5pset_deflate_f(self%pid, self%comp_lvl, ierr)
if (ierr /= 0) error stop 'error enabling Deflate compression '//self%filename

end procedure hdf_set_deflate


module procedure hdf_wrapup
integer :: ierr

call h5sclose_f(self%sid, ierr)
call h5pclose_f(self%pid, ierr)
call h5dclose_f(self%did, ierr)
if (ierr /= 0) error stop 'problem closing dataset in ' // self%filename

end procedure hdf_wrapup


module procedure hdf_open_group

integer :: ierr

call h5gopen_f(self%lid, gname, self%gid, ierr)
if (ierr /= 0) error stop 'problem opening group ' // gname // ' in ' // self%filename

self%glid = self%lid
self%lid  = self%gid

end procedure hdf_open_group


module procedure hdf_close_group

integer :: ierr

call h5gclose_f(self%gid, ierr)
if (ierr /= 0) error stop 'problem closing group '//self%filename

self%lid = self%glid

end procedure hdf_close_group


end submodule write
