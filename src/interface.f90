module h5fortran
!! HDF5 object-oriented polymorphic interface
use, intrinsic :: iso_c_binding, only : c_ptr, c_loc
use, intrinsic :: iso_fortran_env, only : real32, real64, int64, int32, stderr=>error_unit
use hdf5, only : HID_T, SIZE_T, HSIZE_T, &
  H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
  H5F_OBJ_FILE_F, H5F_OBJ_GROUP_F, H5F_OBJ_DATASET_F, H5F_OBJ_DATATYPE_F, H5F_OBJ_ALL_F, &
  H5S_ALL_F, H5S_SELECT_SET_F, &
  H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_COMPACT_F, &
  H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE, &
  H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F, &
  H5F_SCOPE_GLOBAL_F, &
  H5P_DEFAULT_F, &
  h5open_f, h5close_f, &
  h5fopen_f, h5fcreate_f, h5fclose_f, h5fis_hdf5_f, h5fget_filesize_f, &
  h5fget_obj_count_f, h5fget_obj_ids_f, h5fget_name_f, &
  h5dopen_f, h5dclose_f, h5dget_space_f, &
  h5gcreate_f, h5gclose_f, &
  h5iis_valid_f, h5iget_type_f, h5iget_name_f, &
  h5lexists_f, &
  h5sclose_f, h5sselect_hyperslab_f, h5screate_simple_f, &
  h5get_libversion_f, h5eset_auto_f, h5fflush_f, &
  h5tclose_f
use h5lt, only : h5ltget_dataset_ndims_f, h5ltget_dataset_info_f

implicit none (type, external)
private
public :: hdf5_file, hdf5_close, h5write, h5read, h5exist, is_hdf5, h5write_attr, h5read_attr, hdf5version
public :: hdf_shape_check, hdf_rank_check, hdf_get_slice
!! for submodules only
public :: HSIZE_T, HID_T, H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE
public :: H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F
!! HDF5 types for end users

! intrinsic :: size !< workaround for intel fortran

!> main type
type :: hdf5_file

character(:),allocatable  :: filename
integer(HID_T) :: lid=0, &   !< location ID
                  gid, &    !< group ID
                  glid   !< group location ID

logical :: verbose=.true.
logical :: debug=.false.
logical :: fletcher32 = .false.
logical :: shuffle = .false.

integer :: comp_lvl = 0
!! compression level (1-9)  0: disable compression

logical :: use_mpi = .false. !< MPI or serial HDF5


contains
!> define methods (procedures) that don't need generic procedure
procedure, public :: open => hdf_initialize
procedure, public :: close => hdf_finalize
procedure, public :: write_group
procedure, public :: create => hdf_create_user
procedure, public :: open_group => hdf_open_group
procedure, public :: close_group => hdf_close_group
procedure, public :: flush => hdf_flush
procedure, public :: filesize => hdf_filesize
procedure, public :: ndim => hdf_get_ndim
procedure, public :: shape => hdf_get_shape
procedure, public :: layout => hdf_get_layout
procedure, public :: chunks => hdf_get_chunk
procedure, public :: class => get_class
procedure, public :: dtype => get_native_dtype
procedure, public :: deflate => get_deflate
procedure, public :: exist => hdf_check_exist
procedure, public :: exists => hdf_check_exist
procedure, public :: is_contig => hdf_is_contig
procedure, public :: is_chunked => hdf_is_chunked
procedure, public :: is_compact => hdf_is_compact
procedure, public :: softlink => create_softlink
procedure, public :: is_open

!> below are procedure that need generic mapping (type or rank agnostic)

!> write group or dataset integer/real
generic, public :: write => h5write_scalar, h5write_1d, h5write_2d, h5write_3d, h5write_4d, h5write_5d, h5write_6d, h5write_7d

!> write attributes
generic, public :: writeattr => writeattr_char, writeattr_num

!> read attributes
generic, public :: readattr => readattr_char, readattr_num

!> read dataset
generic, public :: read => h5read_scalar, &
h5read_1d, h5read_2d, h5read_3d, h5read_4d, h5read_5d, h5read_6d, h5read_7d

!> private methods
!! each method must be declared here, and above as a generic, public
procedure,private :: h5write_scalar, &
h5write_1d, h5write_2d, h5write_3d, h5write_4d, h5write_5d, h5write_6d, h5write_7d, &
h5read_scalar, h5read_1d, h5read_2d, h5read_3d, h5read_4d, h5read_5d, h5read_6d, h5read_7d, &
writeattr_char, writeattr_num, readattr_char, readattr_num

!> flush file to disk and close file if user forgets to do so.
final :: destructor

end type hdf5_file


interface h5write
procedure lt0write, lt1write, lt2write, lt3write, lt4write, lt5write, lt6write, lt7write
end interface h5write

interface h5read
procedure lt0read, lt1read, lt2read, lt3read, lt4read, lt5read, lt6read, lt7read
end interface h5read

interface h5write_attr
procedure writeattr_num_lt, writeattr_char_lt
end interface h5write_attr

interface h5read_attr
procedure readattr_num_lt, readattr_char_lt
end interface h5read_attr


!> Submodules

interface !< write.f90

module subroutine hdf_create_user(self, dname, dtype, dset_dims, chunk_size, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: dset_dims
integer, intent(in), dimension(:), optional :: chunk_size  !< (:) instead of size(dims) due to intel fortran quirk
logical, intent(in), optional :: compact
end subroutine hdf_create_user

module subroutine hdf_create(self, dname, dtype, mem_dims, dset_dims, filespace_id, dset_id, dtype_id, &
  chunk_size, istart, iend, stride, compact, charlen)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: mem_dims, dset_dims
integer(HID_T), intent(out) :: filespace_id, dset_id
integer(HID_T), intent(out), optional :: dtype_id
integer, intent(in), dimension(:), optional :: chunk_size, istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(in), optional :: charlen !< length of character scalar
!! keep istart, iend, stride for future slice shape check
end subroutine hdf_create

module subroutine hdf_open_group(self, gname)
class(hdf5_file), intent(inout) :: self
character(*), intent(in)        :: gname
end subroutine hdf_open_group

module subroutine hdf_close_group(self)
class(hdf5_file), intent(inout) :: self
end subroutine hdf_close_group

module subroutine write_group(self, group_path)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: group_path   !< full path to group
end subroutine write_group

module subroutine create_softlink(self, tgt, link)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: tgt, &  !< target path to link
                            link  !< soft link path to create
end subroutine create_softlink

module subroutine hdf_flush(self)
!! request operating system flush data to disk.
!! The operating system can do this when it desires, which might be a while.
class(hdf5_file), intent(in) :: self
end subroutine hdf_flush

end interface

interface !< writer_lt.f90

module subroutine lt0write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value
end subroutine lt0write

module subroutine lt1write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:)
end subroutine lt1write

module subroutine lt2write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:)
end subroutine lt2write

module subroutine lt3write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:)
end subroutine lt3write

module subroutine lt4write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:,:)
end subroutine lt4write

module subroutine lt5write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:,:,:)
end subroutine lt5write

module subroutine lt6write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:,:,:,:)
end subroutine lt6write

module subroutine lt7write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:,:,:,:,:)
end subroutine lt7write

end interface


interface !< reader_lt.f90

module logical function h5exist(filename, dname)
character(*), intent(in) :: filename, dname
end function h5exist

module subroutine lt0read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value
end subroutine lt0read

!! NOTE: intent(inout) for non-scalar read is avoid reallocation and
!! segfault with allocatable arrays in user programs,
!! even when the array was already allocated before the h5fortran interface was called.
module subroutine lt1read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:)
end subroutine lt1read

module subroutine lt2read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:)
end subroutine lt2read

module subroutine lt3read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:)
end subroutine lt3read

module subroutine lt4read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:,:)
end subroutine lt4read

module subroutine lt5read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:,:,:)
end subroutine lt5read

module subroutine lt6read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:,:,:,:)
end subroutine lt6read

module subroutine lt7read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:,:,:,:,:)
end subroutine lt7read
end interface

interface !< writer.f90
module subroutine h5write_scalar(self, dname, value, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value
logical, intent(in), optional :: compact
end subroutine h5write_scalar

module subroutine h5write_1d(self, dname, value, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:)
integer, intent(in), dimension(1), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine h5write_1d

module subroutine h5write_2d(self, dname, value, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:)
integer, intent(in), dimension(2), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine h5write_2d

module subroutine h5write_3d(self, dname, value, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:)
integer, intent(in), dimension(3), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine h5write_3d

module subroutine h5write_4d(self, dname, value, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:)
integer, intent(in), dimension(4), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine h5write_4d

module subroutine h5write_5d(self, dname, value, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:)
integer, intent(in), dimension(5), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine h5write_5d

module subroutine h5write_6d(self, dname, value, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), dimension(6), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine h5write_6d

module subroutine h5write_7d(self,dname,value, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), dimension(7), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine h5write_7d

end interface

interface !< read.f90

module integer function get_class(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function get_class

module integer(hid_t) function get_native_dtype(self, dname, ds_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(hid_t), intent(in), optional :: ds_id
end function get_native_dtype

module integer function hdf_get_ndim(self, dname) result (drank)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_get_ndim

module subroutine hdf_get_shape(self, dname, dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(out), allocatable :: dims(:)
end subroutine hdf_get_shape

module logical function get_deflate(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function get_deflate
module integer function hdf_get_layout(self, dname) result(layout)
!! H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_VIRTUAL_F, H5D_COMPACT_F
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_get_layout

module subroutine hdf_get_chunk(self, dname, chunk_size)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(out) :: chunk_size(:)
end subroutine hdf_get_chunk

module logical function hdf_check_exist(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_check_exist
end interface

interface !< reader.f90

module subroutine h5read_scalar(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout)        :: value
end subroutine h5read_scalar

module subroutine h5read_1d(self, dname, value, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:)
integer, intent(in), dimension(1), optional :: istart, iend, stride
end subroutine h5read_1d

module subroutine h5read_2d(self, dname, value, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:)
integer, intent(in), dimension(2), optional :: istart, iend, stride
end subroutine h5read_2d

module subroutine h5read_3d(self, dname, value, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:)
integer, intent(in), dimension(3), optional :: istart, iend, stride
end subroutine h5read_3d

module subroutine h5read_4d(self, dname, value,  istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:)
integer, intent(in), dimension(4), optional :: istart, iend, stride
end subroutine h5read_4d

module subroutine h5read_5d(self, dname, value, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:)
integer, intent(in), dimension(5), optional :: istart, iend, stride
end subroutine h5read_5d

module subroutine h5read_6d(self, dname, value, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:,:)
integer, intent(in), dimension(6), optional :: istart, iend, stride
end subroutine h5read_6d

module subroutine h5read_7d(self, dname, value, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:,:,:)
integer, intent(in), dimension(7), optional :: istart, iend, stride
end subroutine h5read_7d
end interface


interface  !< attributes.f90

module subroutine readattr_char(self, dname, attr, attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
character(*), intent(inout) :: attrval
!! intent(inout) for character
end subroutine readattr_char

module subroutine readattr_num(self, dname, attr, attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
class(*), intent(out) :: attrval(:)
end subroutine readattr_num

module subroutine writeattr_char(self, dname, attr, attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
character(*), intent(in) :: attrval
end subroutine writeattr_char

module subroutine writeattr_num(self, dname, attr, attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
class(*), intent(in) :: attrval(:)
end subroutine writeattr_num

module subroutine writeattr_char_lt(filename, dname, attr, attrval)
character(*), intent(in) :: filename
character(*), intent(in) :: dname, attr
character(*), intent(in) :: attrval
end subroutine writeattr_char_lt

module subroutine writeattr_num_lt(filename, dname, attr, attrval)
character(*), intent(in) :: filename
character(*), intent(in) :: dname, attr
class(*), intent(in) :: attrval(:)
end subroutine writeattr_num_lt

module subroutine readattr_char_lt(filename, dname, attr, attrval)
character(*), intent(in) :: filename
character(*), intent(in) :: dname, attr
character(*), intent(inout) :: attrval
!! intent(inout) for character
end subroutine readattr_char_lt

module subroutine readattr_num_lt(filename, dname, attr, attrval)
character(*), intent(in) :: filename
character(*), intent(in) :: dname, attr
class(*), intent(out) :: attrval(:)
end subroutine readattr_num_lt

end interface

contains


subroutine hdf_initialize(self, filename, action, comp_lvl, shuffle, fletcher32, verbose, debug)
!! Opens hdf5 file

class(hdf5_file), intent(inout)    :: self
character(*), intent(in) :: filename
character(*), intent(in), optional :: action !< r, r+, rw, w, a
integer, intent(in), optional :: comp_lvl  !< 0: no compression. 1-9: ZLIB compression, higher is more compressior
logical, intent(in), optional :: shuffle
logical, intent(in), optional :: fletcher32
logical, intent(in), optional :: verbose, debug

character(:), allocatable :: laction
integer :: ier

if(self%is_open()) then
  write(stderr,*) 'h5fortran:open: file handle already open: '//self%filename
  return
endif

laction = 'rw'
if(present(action)) laction = action

self%filename = filename

if (present(comp_lvl) .and. laction /= "r") self%comp_lvl = comp_lvl
if (present(verbose)) self%verbose = verbose
if (present(debug)) self%debug = debug

if(self%comp_lvl > 0) then
  self%shuffle = .true.
  self%fletcher32 = .true.
endif

if(present(shuffle)) self%shuffle = shuffle
if(present(fletcher32)) self%fletcher32 = fletcher32

if(self%comp_lvl < 0) then
  write(stderr, '(a)') "NOTICE:h5fortran:open: compression level must be >= 0, setting comp_lvl = 0"
  self%comp_lvl = 0
elseif(self%comp_lvl > 9) then
  write(stderr, '(a)') "NOTICE:h5fortran:open: compression level must be <= 9, setting comp_lvl = 9"
  self%comp_lvl = 9
endif

!> Initialize FORTRAN interface.
call h5open_f(ier)
if (ier /= 0) error stop 'ERROR:h5fortran:open: HDF5 library initialize'

if(self%verbose) then
  call h5eset_auto_f(1, ier)
else
  call h5eset_auto_f(0, ier)
endif
if (ier /= 0) error stop 'ERROR:h5fortran:open: HDF5 library set traceback'

select case(laction)
case('r')
  if(.not. is_hdf5(filename)) error stop "ERROR:h5fortran:open: file does not exist: "//filename
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, self%lid,ier)
case('r+')
  if(.not. is_hdf5(filename)) error stop "ERROR:h5fortran:open: file does not exist: "//filename
  call h5fopen_f(filename, H5F_ACC_RDWR_F, self%lid, ier)
case('rw', 'a')
  if(is_hdf5(filename)) then
    call h5fopen_f(filename, H5F_ACC_RDWR_F, self%lid, ier)
  else
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ier)
  endif
case ('w')
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ier)
case default
  error stop 'ERROR:h5fortran:open: Unsupported action: ' // laction
end select

if (ier /= 0) error stop "ERROR:h5fortran:open: HDF5 file open failed: "//filename

end subroutine hdf_initialize


subroutine hdf_finalize(self, close_hdf5_interface)
!! This must be called on each HDF5 file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! We don't reference count because applications might also invoke HDF5
!! directly.
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

class(hdf5_file), intent(in) :: self
logical, intent(in), optional :: close_hdf5_interface

integer :: ierr, i
integer(SIZE_T) :: Ngroup, Ndset, Ndtype, Nfile, Lf_name, Lds_name
integer(HID_T), allocatable :: obj_ids(:)
integer(SIZE_T), parameter :: L = 2048 !< arbitrary length
character(L) :: file_name, dset_name

if (.not. self%is_open()) then
  write(stderr,*) 'WARNING:h5fortran:close: file handle is already closed: '// self%filename
  return
endif

!> ref count for better error messages; this is more of a problem with HDF5-MPI programs
call h5fget_obj_count_f(self%lid, H5F_OBJ_GROUP_F, Ngroup, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open groups: " // self%filename
if(Ngroup > 0) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ngroup, " groups open: " // self%filename


call h5fget_obj_count_f(self%lid, H5F_OBJ_DATASET_F, Ndset, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open datasets: " // self%filename
if(Ndset > 0) then
  write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ndset, " datasets open: " // self%filename

  allocate(obj_ids(Ndset))
  call h5fget_obj_ids_f(self%lid, H5F_OBJ_DATASET_F, Ndset, obj_ids, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_ids: could not get open dataset ids: " // self%filename

  do i = 1, int(Ndset)
    call h5fget_name_f(obj_ids(i), file_name, Lf_name, ierr)
    if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_name: could not get filename of open dataset: " // self%filename

    call h5iget_name_f(obj_ids(i), dset_name, L, Lds_name, ierr)

    write(stderr,*) "h5fortran:close: open dataset: " // dset_name(:Lds_name) // " in file: " // file_name(:Lf_name)
  end do
endif

call h5fget_obj_count_f(self%lid, H5F_OBJ_DATATYPE_F, Ndtype, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open datatypes: " // self%filename
if(Ndtype > 0) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ndtype, " datatypes open: " // self%filename

call h5fget_obj_count_f(self%lid, H5F_OBJ_FILE_F, Nfile, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open files: " // self%filename
if(Nfile < 1) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Nfile, " files open: " // self%filename

if(Ngroup > 0 .or. Ndset > 0 .or. Ndtype > 0) error stop "ERROR:h5fortran:close: hanging HID handles open: " // self%filename


!> close hdf5 file
call h5fclose_f(self%lid, ierr)
if (ierr /= 0) then
  write(stderr,'(a,i0)') 'ERROR:h5fortran:h5fclose: HDF5 file close: ' // self%filename
  error stop
endif

if (present(close_hdf5_interface)) then
  if (close_hdf5_interface) then
    call h5close_f(ierr)
    if (ierr /= 0) error stop 'ERROR:h5fortran:h5close: HDF5 library close'
  endif
endif

end subroutine hdf_finalize


logical function is_open(self)
!! check if file handle is open

class(hdf5_file), intent(in) :: self

! integer :: hid_type
integer :: ierr

call h5iis_valid_f(self%lid, is_open, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:is_open:h5iis_valid: " // self%filename

! call h5iget_type_f(self%lid, hid_type, ierr)
! if(ierr /= 0 .or. hid_type /= H5I_FILE_F) is_open = .false.

end function is_open


subroutine destructor(self)
!! Close file and handle if user forgets to do so

type(hdf5_file), intent(in) :: self

if (.not. self%is_open()) return

print '(a)', "auto-closing " // self%filename
call self%close()

end subroutine destructor


function hdf5version() result(v)
!! tell HDF5 library version (major, minor, release)
integer :: v(3), ierr

!> get library version
call h5get_libversion_f(v(1), v(2), v(3), ierr)
if (ierr/=0) error stop 'ERROR:h5fortran: HDF5 library get version'

end function hdf5version


subroutine hdf5_close()
!! this subroutine will close ALL existing file handles
!! only call it at end of your program
!! "Flushes all data to disk, closes all open identifiers, and cleans up memory."
!! "Should be called by all HDF5 Fortran programs"

integer :: ier

call h5close_f(ier)
if (ier /= 0) error stop 'ERROR: h5fortran:h5close: HDF5 library close'

end subroutine hdf5_close


logical function hdf_is_contig(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
hdf_is_contig = self%layout(dname) == H5D_CONTIGUOUS_F
end function hdf_is_contig

logical function hdf_is_compact(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
hdf_is_compact = self%layout(dname) == H5D_COMPACT_F
end function hdf_is_compact

logical function hdf_is_chunked(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
hdf_is_chunked = self%layout(dname) == H5D_CHUNKED_F
end function hdf_is_chunked


logical function is_hdf5(filename)
!! is this file HDF5?

character(*), intent(in) :: filename
integer :: ierr

inquire(file=filename, exist=is_hdf5)
!! avoid warning/error messages
if (.not. is_hdf5) return

call h5fis_hdf5_f(filename, is_hdf5, ierr)

if (ierr/=0) is_hdf5 = .false.
!! sometimes h5fis_hdf5_f is .true. for missing file

end function is_hdf5


subroutine hdf_get_slice(self, dname, dset_id, filespace_id, memspace_id, i0, i1, i2)
!! setup array slices for read and write
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(inout) :: dset_id  !< inout for sentinel value
integer(HID_T), intent(out) :: filespace_id, memspace_id
integer, intent(in), dimension(:) :: i0
integer, intent(in), dimension(size(i0)) :: i1
integer, intent(in), dimension(size(i0)), optional :: i2

integer(HSIZE_T), dimension(size(i0)) :: istart, iend, stride, mem_dims
integer :: ierr

if(.not.self%is_open()) error stop 'h5fortran:slice: file handle is not open'

istart = int(i0, HSIZE_T)
iend = int(i1, HSIZE_T)

if (present(i2)) then
  stride = int(i2, HSIZE_T)
else
  stride = 1
endif

!! compensate for 0-based hyperslab vs. 1-based Fortran
istart = istart - 1

mem_dims = iend - istart

!> some callers have already opened the dataset. 0 is a sentinel saying not opened yet.
if (dset_id == 0) then
  call h5dopen_f(self%lid, dname, dset_id, ierr)
  if(ierr /= 0) error stop 'h5fortran:get_slice:H5Dopen: ' // dname // ' ' // self%filename
endif

!> Select hyperslab in file
call h5dget_space_f(dset_id, filespace_id, ierr)
if (ierr/=0) error stop "h5dget_space: " // dname

call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, &
start=istart, &
count=mem_dims, &
hdferr=ierr, &
stride=stride)
if(ierr /= 0) error stop 'h5fortran:h5sselect_hyperslab: ' // dname

!> create memory dataspace
call h5screate_simple_f(rank=size(mem_dims), dims=mem_dims, space_id=memspace_id, hdferr=ierr)
if (ierr/=0) error stop "h5fortran:h5screate_simple:memspace " // dname

end subroutine hdf_get_slice


subroutine hdf_rank_check(self, dname, mrank, vector_scalar)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(in) :: mrank
logical, intent(out), optional :: vector_scalar

integer(HSIZE_T) :: ddims(1)
integer(SIZE_T) :: type_size
integer :: ierr, drank, type_class

if(present(vector_scalar)) vector_scalar = .false.

if(.not.self%is_open()) error stop 'h5fortran:rank_check: file handle is not open'

if (.not.self%exist(dname)) error stop 'ERROR: ' // dname // ' does not exist in ' // self%filename

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_dataset_ndims_f(self%lid, dname, drank, ierr)
if (ierr/=0) error stop 'h5fortran:rank_check: get_dataset_ndim ' // dname // ' read ' // self%filename

if (drank == mrank) return

if (present(vector_scalar) .and. drank == 1 .and. mrank == 0) then
  !! check if vector of length 1
  call h5ltget_dataset_info_f(self%lid, dname, dims=ddims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
  if (ierr/=0) error stop 'h5fortran:rank_check: get_dataset_info ' // dname // ' read ' // self%filename
  if (ddims(1) == 1) then
    vector_scalar = .true.
    return
  endif
endif

write(stderr,'(A,I0,A,I0)') 'h5fortran:rank_check: rank mismatch ' // dname // ' = ',drank,'  variable rank =', mrank
error stop


end subroutine hdf_rank_check


subroutine hdf_shape_check(self, dname, dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(in) :: dims(:)

integer :: ierr
integer(SIZE_T) :: type_size
integer(HSIZE_T), dimension(size(dims)):: ddims
integer :: type_class

call hdf_rank_check(self, dname, size(dims))

!> check for matching size, else bad reads can occur.

call h5ltget_dataset_info_f(self%lid, dname, dims=ddims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
if (ierr/=0) error stop 'ERROR:h5fortran:shape_check: get_dataset_info ' // dname // ' read ' // self%filename


if(any(int(dims, int64) /= ddims)) then
  write(stderr,*) 'ERROR:h5fortran:shape_check: shape mismatch ' // dname // ' = ',ddims,'  variable shape =', dims
  error stop
endif

end subroutine hdf_shape_check


integer(HSIZE_T) function hdf_filesize(self)
!! returns the size of the HDF5 file in bytes
class(hdf5_file), intent(in) :: self

integer :: ierr

if(.not. self%is_open()) error stop 'ERROR:h5fortran:filesize: file handle is not open: ' // self%filename

call h5fget_filesize_f(self%lid, hdf_filesize, ierr)
if(ierr/=0) error stop "ERROR:h5fortran: could not get file size " // self%filename

end function hdf_filesize


end module h5fortran
