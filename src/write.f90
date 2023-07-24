submodule (h5fortran) write

use hdf5, only: &
h5fflush_f, &
h5screate_f, h5sclose_f, h5screate_simple_f, H5Sget_simple_extent_ndims_f, H5Sget_simple_extent_dims_f, &
h5dcreate_f, h5dopen_f, h5dclose_f, h5dget_space_f, &
h5pset_chunk_f, h5pset_layout_f, h5pset_deflate_f, h5pset_shuffle_f, h5pset_fletcher32_f, &
h5pcreate_f, h5pclose_f, h5pset_fill_value_f, &
H5P_DATASET_CREATE_F, &
h5gopen_f, h5gcreate_f, h5gclose_f, &
H5Lcreate_soft_f, &
h5tcopy_f, h5tclose_f, h5tset_size_f, &
H5S_SCALAR_F, &
H5D_COMPACT_F, &
H5F_SCOPE_GLOBAL_F


use h5lt, only: h5ltpath_valid_f

implicit none

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

call H5Dclose_f(dset_id, ier)
call estop(ier, "create_user:H5Dclose", self%filename, dname)

call H5Sclose_f(file_space_id, ier)
call estop(ier, "create_user:H5Sclose", self%filename, dname)

call H5Tclose_f(dtype_id, ier)
call estop(ier, "create_user:H5Tclose", self%filename, dname)

end procedure hdf_create_user


module procedure hdf_create

integer :: ier, drank, i
integer(HID_T) :: dcpl
integer(HSIZE_T), dimension(:), allocatable :: ddims, maxdims


call H5Tcopy_f(dtype, dtype_id, ier)
call estop(ier, "create:H5Tcopy", self%filename, dname)

if(dtype == H5T_NATIVE_CHARACTER) then
  if(.not. present(charlen)) error stop "ERROR:h5fortran:hdf_create: character type must specify charlen"
  if (charlen < 1) error stop "ERROR:h5fortran:attr_create: character type must specify charlen > 0"

  call H5Tset_size_f(dtype_id, int(charlen, SIZE_T), ier)
  call estop(ier, "create:H5Tset_size", self%filename, dname)
endif


if(self%exist(dname)) then
  call H5Dopen_f(self%file_id, dname, dset_id, ier)
  call estop(ier, "create:H5Dopen", self%filename, dname)

  call H5Dget_space_f(dset_id, filespace_id, ier)
  call estop(ier, "create:H5Dget_space", self%filename, dname)


  if (present(istart)) then
    if(any(istart < 1)) error stop 'ERROR:h5fortran:create: istart must be >= 1'
    if(any(iend <= istart)) error stop 'ERROR:h5fortran:create: iend must be > istart'

    call H5Sget_simple_extent_ndims_f(filespace_id, drank, ier)
    call estop(ier, "create:H5Sget_simple_extent_ndims", self%filename, dname)

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

  return
endif

!> Only new datasets go past this point
dcpl = H5P_DEFAULT_F

i = index(dname, "/", back=.true.)
if (i>1) call self%create_group(dname(:i-1))
!! create_group is needed for any dataset in a group e.g. /hi/there/var

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
call estop(ier, "create:H5Screate", self%filename, dname)

!> fill value
if(present(fill_value)) then
  if(dcpl == H5P_DEFAULT_F) then
    call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, ier)
    call estop(ier, "create:H5Pcreate", self%filename, dname)
  endif

  select type (fill_value)
  !! dtype_id MUST equal the fill_value type or "transfer()" like bit pattern unexpected data will result
  type is (real(real32))
    call h5pset_fill_value_f(dcpl, H5T_NATIVE_REAL, fill_value, ier)
  type is (real(real64))
    call h5pset_fill_value_f(dcpl, H5T_NATIVE_DOUBLE, fill_value, ier)
  type is (integer(int32))
    call h5pset_fill_value_f(dcpl, H5T_NATIVE_INTEGER, fill_value, ier)
  !! int64 is NOT available for h5pset_fill_value_f
  type is (character(*))
    call h5pset_fill_value_f(dcpl, dtype_id, fill_value, ier)
  class default
    error stop "ERROR:h5fortran:create: unknown fill value type"
  end select
endif

!> create dataset
call h5dcreate_f(self%file_id, dname, type_id=dtype_id, space_id=filespace_id, dset_id=dset_id, hdferr=ier, dcpl_id=dcpl)
call estop(ier, "create:H5Dcreate", self%filename, dname)

!> free resources
call h5pclose_f(dcpl, ier)
call estop(ier, "create:H5Pclose", self%filename, dname)

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
call estop(ier, "set_compact:H5Pcreate", "", dset_name)

call h5pset_layout_f(dcpl, H5D_COMPACT_F, ier)
call estop(ier, "set_compact:H5Pset_layout", "", dset_name)

end subroutine set_compact


module procedure create_softlink
!! HDF5 soft link -- to variables in same file
!! target need not exist (dangling link)
!! linking to external files requires an external link (different function required)

integer :: ier

call H5Lcreate_soft_f(tgt, self%file_id, link, ier)
call estop(ier, "create_softlink:H5Lcreate_soft", self%filename, link)

end procedure create_softlink


module procedure create_group

integer(HID_T)  :: gid
integer :: ier

integer :: j, i, L
character(len_trim(group_path)) :: p, r

if(.not.self%is_open()) error stop 'ERROR:h5fortran:create_group: file handle is not open: ' // self%filename

L = len_trim(group_path)
if(L <= 1) return  !< not a new group

j = 1
r = group_path(j+1:L)
i = index(r, "/")

do while (i > 0 .or. len_trim(r) > 0)
  ! check subgroup exists
  if(i == 0) then
    p = group_path
    r = ""
  else
    j = j + i
    p = group_path(:j-1)
    r = group_path(j+1:L)
    i = index(r, "/")
  endif
  ! print '(a,i0,1x,i0,1x,a)', "TRACE: create_group: ",i,j, group_path // " " // p // " " // r

  if(self % exist(p)) cycle

  call H5Gcreate_f(self%file_id, p, gid, ier)
  call estop(ier, "create_group:H5Gcreate", self%filename, group_path)

  call H5Gclose_f(gid, ier)
  call estop(ier, "create_group:H5Gclose", self%filename, group_path)
end do

end procedure create_group


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

call H5Pcreate_f(H5P_DATASET_CREATE_F, dcpl, ier)
call estop(ier, "set_deflate:H5Pcreate", self%filename, "")

call h5pset_chunk_f(dcpl, size(dims), cs, ier)
call estop(ier, "set_deflate:H5Pset_chunk", self%filename, "")

if (self%fletcher32) then
  !! fletcher32 filter adds a checksum to the data
  call h5pset_fletcher32_f(dcpl, ier)
  call estop(ier, "set_deflate:H5Pset_fletcher32", self%filename, "")
endif

if(self%shuffle) then
  !! shuffle filter improves compression
  call h5pset_shuffle_f(dcpl, ier)
  call estop(ier, "set_deflate:H5Pset_shuffle", self%filename, "")
endif

call h5pset_deflate_f(dcpl, self%comp_lvl, ier)
call estop(ier, "set_deflate:H5Pset_deflate", self%filename, "")

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

call H5Fflush_f(self%file_id, H5F_SCOPE_GLOBAL_F, ier)
call estop(ier, "flush:H5Fflush", self%filename, "")

end procedure hdf_flush


end submodule write
