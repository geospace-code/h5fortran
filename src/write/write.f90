submodule (h5fortran) write
!! This submodule is for writing HDF5 data via child submodules
use hdf5, only: &
h5screate_f, H5S_SCALAR_F, &
h5dcreate_f, &
h5pset_chunk_f, h5pset_layout_f, h5pset_deflate_f, h5pset_shuffle_f, h5pset_fletcher32_f, h5pcreate_f, h5pclose_f, &
H5P_DATASET_CREATE_F, H5P_DEFAULT_F, &
h5gopen_f, &
H5Lcreate_soft_f

use H5LT, only: h5ltmake_dataset_string_f, h5ltpath_valid_f

implicit none (type, external)

contains


module procedure hdf_create

logical :: exists
integer :: ierr
integer(HID_T) :: plist_id, space_id, ds_id

if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open: ' // self%filename

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
!! h5lexists_f can false error with groups--just use h5ltpath_valid
!! stricter than self%exists() since we're creating and/or writing variable
if (ierr /= 0) error stop 'ERROR:h5fortran:create: variable path invalid: ' // dname // ' in ' // self%filename

if(self%debug) print *,'h5fortran:TRACE:create:exists: ' // dname, exists

if(exists) then
  if (.not.present(istart)) then
    if (size(dims) == 0) then
      !! scalar
      call hdf_rank_check(self, dname, size(dims))
    else
      call hdf_shape_check(self, dname, dims)
    endif
  endif
  !! FIXME: read and write slice shape not checked; but should check in future versions

  !> open dataset
  call h5dopen_f(self%lid, dname, ds_id, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:create: could not open ' // dname // ' in ' // self%filename

  if(present(did)) did = ds_id
  if(present(sid)) then
    call h5dget_space_f(ds_id, sid, ierr)
    if(ierr /= 0) error stop 'h5fortran:create could not get dataset ' // dname // ' in ' // self%filename
  end if
  return
endif

if(self%debug) print *, 'h5fortran:TRACE1: ' // dname

!> Only new datasets go past this point

call self%write_group(dname)

!> create properties
plist_id = H5P_DEFAULT_F

if(size(dims) >= 2) then
  if(self%debug) print *, 'h5fortran:TRACE:create: deflate: ' // dname
  call set_deflate(self, dims, plist_id, ierr, chunk_size)
  if (ierr/=0) error stop 'ERROR:h5fortran:create: problem setting deflate on ' // dname
endif

if(present(compact)) then
! print *, "TRACE1: COMPACT", compact
!! don't set COMPACT after CHUNKED, will fail. And it's either or anyway.
if(compact .and. plist_id == H5P_DEFAULT_F .and. product(dims) * 8 < 60000)  then
!! 64000 byte limit, here we assumed 8 bytes / element
  call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, ierr)
  if (check(ierr, self%filename)) error stop "h5fortran:h5pcreate: " // dname

  call h5pset_layout_f(plist_id, H5D_COMPACT_F, ierr)
  if (check(ierr, self%filename)) error stop "h5fortran:h5pset_layout: " // dname
endif
endif

!> create dataspace
if(size(dims) == 0) then
  call h5screate_f(H5S_SCALAR_F, space_id, ierr)
else
  call h5screate_simple_f(size(dims), dims, space_id, ierr)
endif
if (check(ierr, self%filename, dname)) error stop "h5fortran:h5screate: " // dname

!> create dataset
call h5dcreate_f(self%lid, dname, dtype, space_id, ds_id, ierr, plist_id)
if (check(ierr, self%filename, dname)) error stop "h5fortran:h5dcreate: " // dname
call h5pclose_f(plist_id, ierr)
if (check(ierr, self%filename, dname)) error stop "h5fortran:h5pclose: " // dname

if(.not.(present(did) .and. present(sid))) then
  if(self%debug) print *, 'h5fortran:TRACE:create: closing dataset ', dname
  call hdf_wrapup(ds_id, space_id)
endif
if (check(ierr, self%filename, dname)) error stop "h5fortran:wrapup: " // dname

if(present(sid)) sid = space_id
if(present(did)) did = ds_id

end procedure hdf_create


module procedure create_softlink
!! HDF5 soft link -- to variables in same file
!! target need not exist (dangling link)
!! linking to external files requires an external link (different function required)

integer :: ierr

call H5Lcreate_soft_f(target, self%lid, link, ierr)
if (check(ierr, self%filename)) return

end procedure create_softlink


subroutine set_deflate(self, dims, plist_id, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out) :: plist_id
integer, intent(out) :: ierr
integer, intent(in), optional :: chunk_size(:)

integer(HSIZE_T) :: cs(size(dims))

ierr = 0
plist_id = H5P_DEFAULT_F
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

call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, ierr)
if (check(ierr, "h5pcreate: " // self%filename)) return

call h5pset_chunk_f(plist_id, size(dims), cs, ierr)
if (check(ierr, "h5pset_chunk: " // self%filename)) return

call h5pset_shuffle_f(plist_id, ierr)
if (check(ierr, "h5pset_shuffle: " // self%filename)) return

call h5pset_fletcher32_f(plist_id, ierr)
if (check(ierr, "h5pset_fletcher32: " // self%filename)) return

call h5pset_deflate_f(plist_id, self%comp_lvl, ierr)
if (check(ierr, "h5pset_deflate: " // self%filename)) return

if(self%debug) print *,'TRACE:set_deflate done'

end subroutine set_deflate


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
  if (j < 1 .or. j > ndims) error stop 'h5fortran: auto index bounds error'
  chunk_size(j) = ceiling(real(chunk_size(j)) / 2.0)
  i = i+1
end do

end subroutine guess_chunk_size


module procedure hdf_open_group

integer :: ier

if(.not.self%is_open) error stop 'h5fortran:open_group: file handle is not open: ' // self%filename

call h5gopen_f(self%lid, gname, self%gid, ier)
if (check(ier, self%filename, gname)) error stop

self%glid = self%lid
self%lid  = self%gid

end procedure hdf_open_group


module procedure hdf_close_group

integer :: ier

if(.not.self%is_open) error stop 'h5fortran:close_group: file handle is not open: ' // self%filename

call h5gclose_f(self%gid, ier)
if (check(ier, self%filename)) error stop

self%lid = self%glid

end procedure hdf_close_group


end submodule write
