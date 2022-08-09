submodule (h5fortran) write

use hdf5, only: &
h5fflush_f, &
h5screate_f, h5sclose_f, h5screate_simple_f, H5Sget_simple_extent_ndims_f, H5Sget_simple_extent_dims_f, &
h5dcreate_f, h5dopen_f, h5dclose_f, h5dget_space_f, &
h5pset_chunk_f, h5pset_layout_f, h5pset_deflate_f, h5pset_shuffle_f, h5pset_fletcher32_f, &
h5pcreate_f, h5pclose_f, h5pset_fill_value_f, &
H5P_DATASET_CREATE_F, &
h5gopen_f, h5gcreate_f, h5gclose_f, &
H5Lcreate_soft_f, h5lexists_f, &
h5tcopy_f, h5tclose_f, h5tset_size_f, &
H5S_SCALAR_F, &
H5D_COMPACT_F, &
H5F_SCOPE_GLOBAL_F


use h5lt, only: h5ltpath_valid_f

implicit none (type, external)

contains


module procedure hdf_create_user
!! for user %create() method

integer(HID_T) :: file_space_id, dset_id, dtype_id
integer(HSIZE_T), dimension(size(dset_dims)) :: mdims, ddims
integer :: ier

ddims = int(dset_dims, HSIZE_T)

if(present(mem_dims)) then
  mdims = int(mem_dims, HSIZE_T)
else
  mdims = ddims
endif

call hdf_create(self, dname, dtype, mem_dims=mdims, dset_dims=ddims, &
  filespace_id=file_space_id, dset_id=dset_id, dtype_id=dtype_id, compact=compact, &
  charlen=charlen, fill_value=fill_value)

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:write:create_user: closing dataset: " // dname // " in " // self%filename

call h5sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:write:create_user: closing file dataspace: " // dname // " in " // self%filename

if(dtype == H5T_NATIVE_CHARACTER) call h5tclose_f(dtype_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:write:create_user: closing datatype: " // dname // " in " // self%filename

end procedure hdf_create_user


module procedure hdf_create

integer :: ier, drank
integer(HID_T) :: dcpl, type_id
integer(HSIZE_T), dimension(:), allocatable :: ddims, maxdims

if(dtype == H5T_NATIVE_CHARACTER) then
  if(.not. present(charlen)) error stop "h5fortran:hdf_create: character type must specify charlen"
  if(.not. present(dtype_id)) error stop "h5fortran:hdf_create: character type must specify dtype_id"
endif

if(self%exist(dname)) then
  call H5Dopen_f(self%file_id, dname, dset_id, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:create: could not open ' // dname // ' in ' // self%filename

  call H5Dget_space_f(dset_id, filespace_id, ier)
  if(ier /= 0) error stop 'ERROR:h5fortran:create could not get dataset ' // dname // ' in ' // self%filename


  if (present(istart)) then
    if(any(istart < 1)) error stop 'ERROR:h5fortran:create: istart must be >= 1'
    if(any(iend <= istart)) error stop 'ERROR:h5fortran:create: iend must be > istart'

    call H5Sget_simple_extent_ndims_f(filespace_id, drank, ier)
    if(ier /= 0) error stop "ERROR:h5fortran:create: H5Sget_simple_extent_ndims: "  // dname // ' in ' // self%filename

    allocate(ddims(drank), maxdims(drank))

    call H5Sget_simple_extent_dims_f(filespace_id, ddims, maxdims, ier)
    if (ier /= drank) error stop 'ERROR:h5fortran:create: H5Sget_simple_extent_dims: ' // dname // ' in ' // self%filename

    if(any(iend > ddims)) error stop 'ERROR:h5fortran:create: iend > dset_dims'  // dname // ' in ' // self%filename
  else
    if (size(mem_dims) == 0) then
      !! scalar
      call hdf_rank_check(self, dname, filespace_id, size(mem_dims))
    else
      call hdf_shape_check(self, dname, filespace_id, mem_dims)
    endif
  endif

  if(dtype == H5T_NATIVE_CHARACTER) then
    call H5Tcopy_f(dtype, type_id, ier)
    if(ier /= 0) error stop "h5fortran:h5tcopy:character: " // dname // ' in ' // self%filename

    call H5Tset_size_f(type_id, int(charlen, SIZE_T), ier)
    if(ier /= 0) error stop "h5fortran:h5tset_size:character: " // dname // ' in ' // self%filename

    dtype_id = type_id
  endif

  return
endif

!> Only new datasets go past this point
dcpl = H5P_DEFAULT_F

call self%write_group(dname)
!! write_group is needed for any dataset in a group e.g. /hi/there/var

!> compression
if(size(mem_dims) >= 2) then
  if(self%debug) print *, 'h5fortran:TRACE:create: deflate: ' // dname
  call set_deflate(self, mem_dims, dcpl, chunk_size)
endif

if(present(compact)) call set_compact(dcpl, dset_dims, compact, dname)

!> create dataset dataspace
if(size(dset_dims) == 0) then
  call h5screate_f(H5S_SCALAR_F, filespace_id, ier)
else
  call h5screate_simple_f(size(dset_dims), dset_dims, filespace_id, ier)
endif
if (ier /= 0) error stop "ERROR:h5fortran:hdf_create:h5screate:filespace " // dname // " in " // self%filename

!> datatype id and size
if(dtype == H5T_NATIVE_CHARACTER) then
  call h5tcopy_f(dtype, type_id, ier)
  if(ier /= 0) error stop "h5fortran:h5tcopy:character: " // dname // " in " // self%filename

  call h5tset_size_f(type_id, int(charlen, SIZE_T), ier)
  if(ier /= 0) error stop "h5fortran:h5tset_size:character: " // dname // " in " // self%filename

  dtype_id = type_id
else
  type_id = dtype
endif

!> fill value
if(present(fill_value)) then
  if(dcpl == H5P_DEFAULT_F) then
    call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, ier)
    if (ier /= 0) error stop "ERROR:h5fortran:set_fill:h5pcreate: " // self%filename
  endif

  select type (fill_value)
  !! type_id MUST equal the fill_value type or "transfer()" like bit pattern unexpected data will result
  type is (real(real32))
    call h5pset_fill_value_f(dcpl, H5T_NATIVE_REAL, fill_value, ier)
  type is (real(real64))
    call h5pset_fill_value_f(dcpl, H5T_NATIVE_DOUBLE, fill_value, ier)
  type is (integer(int32))
    call h5pset_fill_value_f(dcpl, H5T_NATIVE_INTEGER, fill_value, ier)
  !! int64 is NOT available for h5pset_fill_value_f
  type is (character(*))
    call h5pset_fill_value_f(dcpl, type_id, fill_value, ier)
  class default
    error stop "ERROR:h5fortran:create: unknown fill value type"
  end select
endif

!> create dataset
call h5dcreate_f(self%file_id, dname, type_id=type_id, space_id=filespace_id, dset_id=dset_id, hdferr=ier, dcpl_id=dcpl)
if (ier /= 0) error stop "ERROR:h5fortran:hdf_create:h5dcreate: " // dname // " in " // self%filename

!> free resources
call h5pclose_f(dcpl, ier)
if (ier /= 0) error stop "ERROR:h5fortran:h5pclose: " // dname // ' in ' // self%filename

end procedure hdf_create


subroutine set_compact(dcpl, dset_dims, compact, dset_name)
!! compact dataset (for very small datasets to increase I/O speed)

integer(HID_T), intent(inout) :: dcpl
integer(HSIZE_T), intent(in) :: dset_dims(:)
logical, intent(in) :: compact
character(*), intent(in) :: dset_name

integer :: ier
integer(HSIZE_T) :: Nbytes

if(.not. compact) return

if(dcpl /= H5P_DEFAULT_F) return
!! datasets are EITHER compact or chunked.

Nbytes = product(dset_dims) * 8
if(Nbytes > 60000) then
  write(stderr,'(a,i0,1x,a)') "WARNING:h5fortran:set_compact: dataset is too large to be compact: bytes: ", Nbytes, dset_name
  return
endif
!! 64000 byte limit, here we assumed 8 bytes / element

call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, ier)
if (ier /= 0) error stop "ERROR:h5fortran:hdf_create:h5pcreate:set_compact"

call h5pset_layout_f(dcpl, H5D_COMPACT_F, ier)
if (ier /= 0) error stop "ERROR:h5fortran:hdf_create:h5pset_layout:set_compact"

end subroutine set_compact


module procedure create_softlink
!! HDF5 soft link -- to variables in same file
!! target need not exist (dangling link)
!! linking to external files requires an external link (different function required)

integer :: ier

call H5Lcreate_soft_f(tgt, self%file_id, link, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:create_softlink: ' // link // ' in ' // self%filename

end procedure create_softlink


module procedure write_group

integer(HID_T)  :: gid
integer :: ier

integer :: sp, ep, L
logical :: gexist

if(.not.self%is_open()) error stop 'ERROR:h5fortran:write_group: file handle is not open'

L = len_trim(group_path)
if(L < 2) return  !< not a new group

sp = 1
ep = 0

grps: do
  ep = index(group_path(sp+1:L), "/")
  if (ep == 0) return

  ! check subgroup exists
  sp = sp + ep
  call h5lexists_f(self%file_id, group_path(:sp-1), gexist, ier)
  if (ier /= 0) error stop "ERROR:h5fortran:write_group: check exists group " // group_path // " in " // self%filename

  if(gexist) cycle grps

  call h5gcreate_f(self%file_id, group_path(:sp-1), gid, ier)
  if (ier /= 0) error stop "ERROR:h5fortran:write_group: create group " // group_path // " in " // self%filename

  call h5gclose_f(gid, ier)
  if (ier /= 0) error stop "ERROR:h5fortran:write_group: close new group " // group_path // " in " // self%filename
end do grps

end procedure write_group


subroutine set_deflate(self, dims, dcpl, chunk_size)
class(hdf5_file), intent(in) :: self
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out) :: dcpl
integer, intent(in), optional :: chunk_size(:)

integer(HSIZE_T) :: cs(size(dims))
integer :: ier


dcpl = H5P_DEFAULT_F

if (present(chunk_size)) then
  if(size(chunk_size) /= size(dims)) then
    write(stderr,*) "ERROR:h5fortran:write:set_deflate: chunk_size length ", size(chunk_size), " /= dims length ", size(dims)
    error stop
  endif
  cs = chunk_size
  where (cs > dims) cs = dims
  if(self%debug) print *,'TRACE: user request chunk_size ',cs
elseif (self%comp_lvl < 1 .or. self%comp_lvl > 9) then
  ! didn't request chunk_size and didn't request compression
  return
else
  !! guess chunk size, keeping in mind 1 Megabyte recommended maximum chunk size
  call guess_chunk_size(dims, cs)
endif

if(self%debug) print *,'DEBUG:set_deflate: dims: ',dims,'chunk size: ', cs

if(any(cs == 0)) return  !< array too small to chunk
if(any(cs < 0)) error stop "ERROR:h5fortran:set_deflate: chunk_size must be strictly positive"

call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, ier)
if (ier /= 0) error stop "ERROR:h5fortran:set_deflate:h5pcreate: " // self%filename

call h5pset_chunk_f(dcpl, size(dims), cs, ier)
if (ier /= 0) error stop "ERROR:h5fortran:set_deflate:h5pset_chunk: " // self%filename

if (self%fletcher32) then
  !! fletcher32 filter adds a checksum to the data
  call h5pset_fletcher32_f(dcpl, ier)
  if (ier /= 0) error stop "ERROR:h5fortran:set_deflate:h5pset_fletcher32: " // self%filename
endif

if(self%shuffle) then
  !! shuffle filter improves compression
  call h5pset_shuffle_f(dcpl, ier)
  if (ier /= 0) error stop "ERROR:h5fortran:set_deflate:h5pset_shuffle: " // self%filename
endif

call h5pset_deflate_f(dcpl, self%comp_lvl, ier)
if (ier/=0) error stop "ERROR:h5fortran:set_deflate:h5pset_deflate: " // self%filename

if(self%debug) print '(a,i0)','TRACE:set_deflate done, comp_lvl: ', self%comp_lvl

end subroutine set_deflate


pure subroutine guess_chunk_size(dims, chunk_size)
!! if array is too small to chunk, returns 0.

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


chunk_size = 0

if (product(dims) * TYPESIZE < CHUNK_MIN) return

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
  if (j < 1 .or. j > ndims) error stop 'ERROR:h5fortran:guess_chunk_size: auto index bounds error'
  chunk_size(j) = ceiling(real(chunk_size(j)) / 2.0)
  i = i+1
end do

end subroutine guess_chunk_size


module procedure hdf_flush

integer :: ier

call h5fflush_f(self%file_id, H5F_SCOPE_GLOBAL_F, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:flush ' // self%filename

end procedure hdf_flush


end submodule write
