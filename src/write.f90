!! This submodule is for writing HDF5 data via child submodules
submodule (hdf5_interface) write

use H5LT, only: h5screate_f, h5sclose_f, h5dcreate_f, h5dwrite_f, h5dclose_f, h5ltmake_dataset_f, &
  h5ltpath_valid_f, h5ltset_attribute_string_f, h5screate_simple_f, &
  h5pset_chunk_f, h5pset_deflate_f, h5pset_shuffle_f, h5pcreate_f, H5P_DATASET_CREATE_F, h5pclose_f, &
  h5ltmake_dataset_string_f, h5gopen_f, h5gclose_f
implicit none
contains


module procedure writeattr

logical :: exists

call self%write(dname, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' ' // self%filename
  return
endif

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: checking existence: ' // dname // ' file ' // self%filename
  ierr = -1
  return
endif

if (.not.exists) then
  write(stderr,*) 'ERROR: variable ' // dname // ' must be created before writing ' // attr
  return
endif

call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: writing attribute ' // attr // ' to ' // dname // ' file ' // self%filename
  return
endif

end procedure writeattr


module procedure hdf_write_string
!! subroutine hdf_write_string(self, dname, value)

call h5ltmake_dataset_string_f(self%lid, dname, value, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: dataset ' // dname // ' write ' // self%filename
  return
endif

end procedure hdf_write_string


module procedure hdf_setup_write

call self%write(dname, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' ' // self%filename
  return
endif

if (present(chunk_size)) self%chunk_size(:size(dims)) = chunk_size

call hdf_set_deflate(self, dims, ierr)

call h5screate_simple_f(size(dims), dims, self%sid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: dataspace ' // dname // ' ' // self%filename
  return
endif

call h5dcreate_f(self%lid, dname, dtype, self%sid, self%did, ierr, self%pid)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: dataset ' // dname // ' ' // self%filename
  return
endif

end procedure hdf_setup_write


module procedure hdf_set_deflate

integer :: ndims, i
integer(HSIZE_T), allocatable :: chunk_size(:)


ndims = size(dims)
allocate(chunk_size(ndims))

do i=1,ndims
  chunk_size(i) = min(self%chunk_size(i), dims(i))
enddo

if (self%verbose) print *,'dims: ',dims,'chunk size: ',chunk_size

call h5pcreate_f(H5P_DATASET_CREATE_F, self%pid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'error creating property ' // self%filename
  return
endif

call h5pset_chunk_f(self%pid, ndims, chunk_size, ierr)
if (ierr /= 0) then
  write(stderr,*) 'error setting chunk ' // self%filename
  return
endif

if (self%comp_lvl < 1 .or. self%comp_lvl > 9) return

call h5pset_shuffle_f(self%pid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'error enabling Shuffle ' // self%filename
  return
endif

call h5pset_deflate_f(self%pid, self%comp_lvl, ierr)
if (ierr /= 0) then
  write(stderr,*) 'error enabling Deflate compression ' // self%filename
  return
endif

end procedure hdf_set_deflate


module procedure hdf_wrapup

call h5sclose_f(self%sid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'problem closing dataspace in ' // self%filename
  return
endif

call h5pclose_f(self%pid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'problem closing data property in ' // self%filename
  return
endif

call h5dclose_f(self%did, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: closing dataset in ' // self%filename
  return
endif

end procedure hdf_wrapup


module procedure hdf_open_group

call h5gopen_f(self%lid, gname, self%gid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: opening group ' // gname // ' in ' // self%filename
  return
endif

self%glid = self%lid
self%lid  = self%gid

end procedure hdf_open_group


module procedure hdf_close_group

call h5gclose_f(self%gid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: closing group '//self%filename
  return
endif

self%lid = self%glid

end procedure hdf_close_group


end submodule write
