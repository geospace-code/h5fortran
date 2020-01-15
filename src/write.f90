!! This submodule is for writing HDF5 data via child submodules
submodule (h5fortran) write

use hdf5, only: &
h5screate_f, h5sclose_f, h5screate_simple_f, H5S_SCALAR_F, &
h5dopen_f, h5dcreate_f, h5dwrite_f, h5dclose_f, &
h5pset_chunk_f, h5pset_deflate_f, h5pset_shuffle_f, h5pcreate_f, H5P_DATASET_CREATE_F, h5pclose_f, &
h5gopen_f, h5gclose_f

use H5LT, only: h5ltmake_dataset_f, h5ltpath_valid_f, h5ltset_attribute_string_f, h5ltmake_dataset_string_f

implicit none
contains

module procedure writeattr

logical :: exists

call self%write(dname, ierr)
if (check(ierr,  'ERROR: create ' // dname // ' ' // self%filename)) return

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (check(ierr,  'ERROR: checking existence: ' // dname // ' file ' // self%filename)) return

if (.not.exists) then
  write(stderr,*) 'ERROR: variable ' // dname // ' must be created before writing ' // attr
  return
endif

call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ierr)
if (ierr /= 0) write(stderr,*) 'ERROR: writing attribute ' // attr // ' to ' // dname // ' file ' // self%filename

end procedure writeattr


module procedure hdf_setup_write
!! hdf_setup_write(self, dname, dtype, dims, sid, did, ierr, chunk_size)

logical :: exists
integer(HID_T) :: pid

!! sentinel values
pid = 0
sid = 0

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (check(ierr,  'ERROR: setup_write: ' // dname // ' check exist ' // self%filename)) return

if(exists) then
  !> open dataset
  call h5dopen_f(self%lid, dname, did, ierr)
  if (check(ierr, 'ERROR: setup_write: open ' // dname // ' ' // self%filename)) return
  return
else
  call self%write(dname, ierr)
  if (check(ierr, 'ERROR: setup_write: create ' // dname // ' ' // self%filename)) return
endif

if(size(dims) >= 2) call hdf_set_deflate(self, dims, pid, ierr, chunk_size)

if(size(dims) == 0) then
  call h5screate_f(H5S_SCALAR_F, sid, ierr)
else
  call h5screate_simple_f(size(dims), dims, sid, ierr)
endif
if (check(ierr,  'ERROR: setup_write: dataspace ' // dname // ' create ' // self%filename)) return

if(pid == 0) then
  call h5dcreate_f(self%lid, dname, dtype, sid, did, ierr)
else
  call h5dcreate_f(self%lid, dname, dtype, sid, did, ierr, pid)
  if (check(ierr, 'ERROR: setup_write: create ' // dname // ' property: ' // self%filename)) return
  call h5pclose_f(pid, ierr)
  if (check(ierr, 'ERROR: setup_write: close property: ' // self%filename)) return
endif
if (check(ierr,  'ERROR: setup_write: dataset ' // dname // ' create ' // self%filename)) return

end procedure hdf_setup_write


subroutine hdf_set_deflate(self, dims, pid, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out) :: pid
integer, intent(out) :: ierr
integer, intent(in), optional :: chunk_size(:)

integer :: i
integer(HSIZE_T) :: cs(size(dims))

pid = 0
if (self%comp_lvl < 1 .or. self%comp_lvl > 9) return
if (.not.present(chunk_size) .and. all(self%chunk_size == 1) .or. any(self%chunk_size < 1)) return
if (present(chunk_size)) then
  if (any(chunk_size < 1)) return
endif

if (present(chunk_size)) then
  cs = chunk_size
else
  do i = 1,size(dims)
    cs(i) = min(self%chunk_size(i), dims(i))
  enddo
endif

! print *,'dims: ',dims,'chunk size: ', cs

call h5pcreate_f(H5P_DATASET_CREATE_F, pid, ierr)
if (check(ierr, 'ERROR: create property ' // self%filename)) return

call h5pset_chunk_f(pid, size(dims), cs, ierr)
if (check(ierr, 'ERROR: set chunk ' // self%filename)) return

if (self%comp_lvl < 1 .or. self%comp_lvl > 9) return

call h5pset_shuffle_f(pid, ierr)
if (check(ierr, 'ERROR: enable Shuffle ' // self%filename)) return

call h5pset_deflate_f(pid, self%comp_lvl, ierr)
if (check(ierr, 'ERROR: enable Deflate compression ' // self%filename)) return

end subroutine hdf_set_deflate


subroutine hdf_wrapup(did, sid, ierr)
integer(HID_T), intent(in) :: sid, did
integer, intent(out) :: ierr

if(sid /= 0) then
  call h5sclose_f(sid, ierr)
  if (check(ierr, 'ERROR: close dataspace')) return
endif

call h5dclose_f(did, ierr)
if (check(ierr, 'ERROR: close dataset')) return

end subroutine hdf_wrapup


module procedure hdf_open_group

call h5gopen_f(self%lid, gname, self%gid, ierr)
if (check(ierr, 'ERROR: opening group ' // gname // ' in ' // self%filename)) return

self%glid = self%lid
self%lid  = self%gid

end procedure hdf_open_group


module procedure hdf_close_group

call h5gclose_f(self%gid, ierr)
if (check(ierr,  'ERROR: closing group '//self%filename)) return

self%lid = self%glid

end procedure hdf_close_group


end submodule write
