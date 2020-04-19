submodule (h5fortran) write
!! This submodule is for writing HDF5 data via child submodules
use hdf5, only: &
h5screate_f, H5S_SCALAR_F, &
h5dcreate_f, &
h5pset_chunk_f, h5pset_deflate_f, h5pset_shuffle_f, h5pset_fletcher32_f, h5pcreate_f, H5P_DATASET_CREATE_F, h5pclose_f, &
h5gopen_f

use H5LT, only: h5ltpath_valid_f, h5ltset_attribute_string_f, h5ltmake_dataset_string_f

implicit none (type, external)

contains

module procedure writeattr

integer :: ier

!call self%write(dname, ier)

! if (ier == 0) call h5ltpath_valid_f(self%lid, dname, .true., exists, ier)

! if (.not.exists) then
!   write(stderr,*) 'ERROR: variable ' // dname // ' must be created before writing ' // attr
!   ier = -1
! endif

call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR:writeattr ' // dname // ' ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

end procedure writeattr


subroutine hdf_setup_write(self, dname, dtype, dims, sid, did, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out) :: sid, did
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr

logical :: exists
integer(HID_T) :: pid

!! sentinel values
pid = 0
sid = 0

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (check(ierr,  'ERROR: setup_write: ' // dname // ' check exist ' // self%filename)) return

if(exists) then
  call hdf_shape_check(self, dname, dims, ierr)
  if (ierr/=0) return
  !> open dataset
  call h5dopen_f(self%lid, dname, did, ierr)
  if (check(ierr, 'ERROR:setup_write: open ' // dname // ' ' // self%filename)) return
  return
else
  call self%write_group(dname, ierr)
  if (check(ierr, 'ERROR:setup_write: create ' // dname // ' ' // self%filename)) return
endif

if(size(dims) >= 2) then
  if(self%debug) print *, 'DEBUG:setup_write: deflate: ' // dname
  call hdf_set_deflate(self, dims, pid, ierr, chunk_size)
endif

if(size(dims) == 0) then
  call h5screate_f(H5S_SCALAR_F, sid, ierr)
else
  call h5screate_simple_f(size(dims), dims, sid, ierr)
endif
if (check(ierr,  'ERROR:setup_write: dataspace ' // dname // ' create ' // self%filename)) return

if(pid == 0) then
  call h5dcreate_f(self%lid, dname, dtype, sid, did, ierr)
else
  call h5dcreate_f(self%lid, dname, dtype, sid, did, ierr, pid)
  if (check(ierr, 'ERROR:setup_write: create ' // dname // ' property: ' // self%filename)) return
  call h5pclose_f(pid, ierr)
  if (check(ierr, 'ERROR:setup_write: close property: ' // self%filename)) return
endif
if (check(ierr,  'ERROR:setup_write: dataset ' // dname // ' create ' // self%filename)) return

end subroutine hdf_setup_write


subroutine hdf_set_deflate(self, dims, pid, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out) :: pid
integer, intent(out) :: ierr
integer, intent(in), optional :: chunk_size(:)

integer(HSIZE_T) :: cs(size(dims))

pid = 0
if (self%comp_lvl < 1 .or. self%comp_lvl > 9) return

if (present(chunk_size)) then
  cs = chunk_size
  where (cs > dims) cs = dims
  if(self%debug) print *,'TRACE: user request chunk_size ',cs
else
  !! guess chunk size, keeping in mind 1 Megabyte recommended maximum chunk size
  call guess_chunk_size(dims, cs)
endif

if(any(cs < 1)) return

if(self%debug) print *,'DEBUG:set_deflate: dims: ',dims,'chunk size: ', cs

call h5pcreate_f(H5P_DATASET_CREATE_F, pid, ierr)
if (check(ierr, 'ERROR: create property ' // self%filename)) return

call h5pset_chunk_f(pid, size(dims), cs, ierr)
if (check(ierr, 'ERROR: set chunk ' // self%filename)) return

call h5pset_shuffle_f(pid, ierr)
if (check(ierr, 'ERROR: enable Shuffle ' // self%filename)) return

call h5pset_fletcher32_f(pid, ierr)
if (check(ierr, 'ERROR: enable Fletcher32 checksum ' // self%filename)) return

call h5pset_deflate_f(pid, self%comp_lvl, ierr)
if (check(ierr, 'ERROR: enable Deflate compression ' // self%filename)) return

if(self%debug) print *,'TRACE: set_deflate done'

end subroutine hdf_set_deflate


subroutine guess_chunk_size(dims, chunk_size)
!! based on https://github.com/h5py/h5py/blob/master/h5py/_hl/filters.py
!! refer to https://support.hdfgroup.org/HDF5/Tutor/layout.html
integer(HSIZE_T), intent(in) :: dims(:)
integer(HSIZE_T), intent(out) :: chunk_size(:)

integer(hsize_t), parameter :: &
CHUNK_BASE = 16000, &    !< Multiplier by which chunks are adjusted
CHUNK_MIN = 8000, &      !< lower limit: 8 kbyte
CHUNK_MAX = 1000000, &   !< upper limit: 1 Mbyte
TYPESIZE = 8             !< bytes, assume real64 for simplicity

integer(hsize_t) :: dset_size, target_size, chunk_bytes, i, j, ndims

if (product(dims) * TYPESIZE < CHUNK_MIN) then
  chunk_size = 0
  return
endif

ndims = size(chunk_size)
chunk_size = dims

dset_size = product(chunk_size) * TYPESIZE
target_size = int(CHUNK_BASE * (2**log10(real(dset_size) / 1e6)), hsize_t)
if (target_size > CHUNK_MAX) target_size = CHUNK_MAX

! print *,'target_size [bytes]: ',target_size

i = 0
do
  !! Repeatedly loop over the axes, dividing them by 2.
  !! Stop when:
  !!   1a. We're smaller than the target chunk size, OR
  !!   1b. We're within 50% of the target chunk size, AND
  !!    2. The chunk is smaller than the maximum chunk size

  chunk_bytes = product(chunk_size) * TYPESIZE

  if ((chunk_bytes < target_size .or. 2*(abs(chunk_bytes-target_size) / target_size) < 1) .and. &
     chunk_bytes < CHUNK_MAX) exit

  if (product(chunk_size) == 1) exit
  !! Element size larger than CHUNK_MAX
  j = int(modulo(i, ndims), hsize_t) + 1
  if (j < 1 .or. j > ndims) error stop 'auto index bounds error'
  chunk_size(j) = ceiling(real(chunk_size(j)) / 2.0)
  i = i+1
end do

end subroutine guess_chunk_size


module procedure hdf_open_group

integer :: ier
call h5gopen_f(self%lid, gname, self%gid, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: opening group ' // gname // ' in ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

self%glid = self%lid
self%lid  = self%gid

end procedure hdf_open_group


module procedure hdf_close_group

integer :: ier

call h5gclose_f(self%gid, ier)

if (present(ierr)) ierr = ier
if (check(ier,  'ERROR: closing group '//self%filename)) then
  if (present(ierr)) return
  error stop
endif

self%lid = self%glid

end procedure hdf_close_group


end submodule write
