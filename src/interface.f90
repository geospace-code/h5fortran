module h5fortran
!! HDF5 object-oriented polymorphic interface
use, intrinsic :: iso_c_binding, only : c_ptr, c_loc
use, intrinsic :: iso_fortran_env, only : real32, real64, int64, int32, stderr=>error_unit
use hdf5, only : HID_T, SIZE_T, HSIZE_T, H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
  H5S_ALL_F, H5S_SELECT_SET_F, &
  H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_COMPACT_F, &
  H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5F_SCOPE_GLOBAL_F, &
  h5open_f, h5close_f, &
  h5dopen_f, h5dclose_f, h5dget_space_f, &
  h5gcreate_f, h5gclose_f, &
  h5fopen_f, h5fcreate_f, h5fclose_f, h5fis_hdf5_f, &
  h5lexists_f, &
  h5sclose_f, h5sselect_hyperslab_f, h5screate_simple_f, &
  h5get_libversion_f, h5eset_auto_f, h5fflush_f
use h5lt, only : h5ltget_dataset_ndims_f, h5ltget_dataset_info_f

implicit none (type, external)
private
public :: hdf5_file, hdf5_close, h5write, h5read, h5exist, is_hdf5, h5write_attr, h5read_attr, &
  check, hdf_shape_check, hdf_rank_check, hdf_get_slice, hdf_wrapup, & !< for submodules only
  HSIZE_T, HID_T, H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER !< HDF5 types for end users

!> main type
type :: hdf5_file

character(:),allocatable  :: filename
integer(HID_T) :: lid=0, &   !< location ID
                  gid, &    !< group ID
                  glid   !< group location ID

integer :: comp_lvl = 0 !< compression level (1-9)  0: disable compression
logical :: verbose=.true.
logical :: debug=.false.
logical :: is_open = .false.
integer :: libversion(3)  !< major, minor, rel

logical :: use_mpi = .false. !< MPI or serial HDF5


contains
!> define methods (procedures) that don't need generic procedure
procedure, public :: initialize => hdf_initialize, open => hdf_initialize, &
  finalize => hdf_finalize, close => hdf_finalize, &
  write_group, create => hdf_create, &
  open_group => hdf_open_group, close_group => hdf_close_group, &
  flush => hdf_flush, &
  ndims => hdf_get_ndims, &
  shape => hdf_get_shape, layout => hdf_get_layout, chunks => hdf_get_chunk, &
  exist => hdf_check_exist, exists => hdf_check_exist, &
  is_contig => hdf_is_contig, is_chunked => hdf_is_chunked, is_compact => hdf_is_compact, &
  softlink => create_softlink

!> below are procedure that need generic mapping (type or rank agnostic)

!> write group or dataset integer/real
generic, public :: write => &
hdf_write_scalar_r32,hdf_write_scalar_r64,hdf_write_scalar_i32,hdf_write_scalar_i64, hdf_write_scalar_char, &
hdf_write_1d_r32, hdf_write_1d_r64, hdf_write_1d_i32, hdf_write_1d_i64, &
hdf_write_2d_r32, hdf_write_2d_r64, hdf_write_2d_i32, hdf_write_2d_i64, &
hdf_write_3d_r32, hdf_write_3d_r64, hdf_write_3d_i32, hdf_write_3d_i64, &
hdf_write_4d_r32, hdf_write_4d_r64, hdf_write_4d_i32, hdf_write_4d_i64, &
hdf_write_5d_r32, hdf_write_5d_r64, hdf_write_5d_i32, hdf_write_5d_i64, &
hdf_write_6d_r32, hdf_write_6d_r64, hdf_write_6d_i32, hdf_write_6d_i64, &
hdf_write_7d_r32, hdf_write_7d_r64, hdf_write_7d_i32, hdf_write_7d_i64

generic, public :: write_r32 => hdf_write_scalar_r32,hdf_write_1d_r32,hdf_write_2d_r32,hdf_write_3d_r32, &
hdf_write_4d_r32,hdf_write_5d_r32,hdf_write_6d_r32,hdf_write_7d_r32

generic, public :: write_r64 => hdf_write_scalar_r64,hdf_write_1d_r64,hdf_write_2d_r64,hdf_write_3d_r64, &
hdf_write_4d_r64,hdf_write_5d_r64,hdf_write_6d_r64,hdf_write_7d_r64

generic, public :: write_i32 => hdf_write_scalar_i32,hdf_write_1d_i32,hdf_write_2d_i32,hdf_write_3d_i32, &
hdf_write_4d_i32,hdf_write_5d_i32,hdf_write_6d_i32,hdf_write_7d_i32

generic, public :: write_i64 => hdf_write_scalar_i64,hdf_write_1d_i64,hdf_write_2d_i64,hdf_write_3d_i64, &
hdf_write_4d_i64,hdf_write_5d_i64,hdf_write_6d_i64,hdf_write_7d_i64

generic, public :: write_char => hdf_write_scalar_char

!> write attributes
generic, public :: writeattr => writeattr_char, writeattr_num

!> read attributes
generic, public :: readattr => readattr_char, readattr_num

!> read dataset
generic, public :: read => &
hdf_read_scalar_char, hdf_read_scalar_r32, hdf_read_scalar_r64, hdf_read_scalar_i32, hdf_read_scalar_i64, &
hdf_read_1d, hdf_read_2d, hdf_read_3d, hdf_read_4d,hdf_read_5d, hdf_read_6d, hdf_read_7d

!> private methods
!! each method must be declared here, and above as a generic, public
procedure,private :: &
hdf_write_scalar_r32, hdf_write_scalar_r64, hdf_write_scalar_i32, hdf_write_scalar_i64, hdf_write_scalar_char, &
hdf_write_1d_r32, hdf_write_1d_r64, hdf_write_1d_i32, hdf_write_1d_i64, &
hdf_write_2d_r32, hdf_write_2d_r64, hdf_write_2d_i32, hdf_write_2d_i64, &
hdf_write_3d_r32, hdf_write_3d_r64, hdf_write_3d_i32, hdf_write_3d_i64, &
hdf_write_4d_r32, hdf_write_4d_r64, hdf_write_4d_i32, hdf_write_4d_i64, &
hdf_write_5d_r32, hdf_write_5d_r64, hdf_write_5d_i32, hdf_write_5d_i64, &
hdf_write_6d_r32, hdf_write_6d_r64, hdf_write_6d_i32, hdf_write_6d_i64, &
hdf_write_7d_r32, hdf_write_7d_r64, hdf_write_7d_i32, hdf_write_7d_i64, &
hdf_read_scalar_char, hdf_read_scalar_r32, hdf_read_scalar_r64, hdf_read_scalar_i32, hdf_read_scalar_i64, &
hdf_read_1d, hdf_read_2d, hdf_read_3d, hdf_read_4d, hdf_read_5d, hdf_read_6d, hdf_read_7d, &
writeattr_char, writeattr_num, readattr_char, readattr_num

!> flush file to disk and close file if user forgets to do so.
final :: destructor

end type hdf5_file


interface h5write
procedure lt0write_r32, lt0write_r64, lt0write_i32, lt0write_i64, lt0write_char, &
lt1write_r32, lt1write_r64, lt1write_i32, lt1write_i64, &
lt2write_r32, lt2write_r64, lt2write_i32, lt2write_i64, &
lt3write_r32, lt3write_r64, lt3write_i32, lt3write_i64, &
lt4write_r32, lt4write_r64, lt4write_i32, lt4write_i64, &
lt5write_r32, lt5write_r64, lt5write_i32, lt5write_i64, &
lt6write_r32, lt6write_r64, lt6write_i32, lt6write_i64, &
lt7write_r32, lt7write_r64, lt7write_i32, lt7write_i64
end interface h5write

interface h5read
procedure lt0read_r32, lt0read_r64, lt0read_i32, lt0read_i64, lt0read_char, &
lt1read, lt2read, lt3read, lt4read, lt5read, lt6read, lt7read
end interface h5read

interface h5write_attr
procedure writeattr_num_lt, writeattr_char_lt
end interface h5write_attr

interface h5read_attr
procedure readattr_num_lt, readattr_char_lt
end interface h5read_attr


!> Submodules

interface !< pathlib.f90
module subroutine std_unlink(filename)
character(*), intent(in) :: filename
end subroutine std_unlink

module logical function is_absolute_path(path)
character(*), intent(in) :: path
end function is_absolute_path

module function get_tempdir()
character(:), allocatable :: get_tempdir
end function

end interface

interface !< write.f90
module subroutine hdf_create(self, dname, dtype, dims, sid, did, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out), optional :: sid, did
integer, intent(in), optional :: chunk_size(:), istart(:), iend(:), stride(:)
logical, intent(in), optional :: compact
!! keep istart, iend, stride for future slice shape check
end subroutine hdf_create

module subroutine hdf_open_group(self, gname, ierr)
class(hdf5_file), intent(inout) :: self
character(*), intent(in)        :: gname
integer, intent(out), optional :: ierr
end subroutine hdf_open_group

module subroutine hdf_close_group(self, ierr)
class(hdf5_file), intent(inout) :: self
integer, intent(out), optional :: ierr
end subroutine hdf_close_group

module subroutine create_softlink(self, target, link)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: target, &  !< target path to link
                            link  !< soft link path to create
end subroutine create_softlink

end interface

interface !< writer_lt.f90
module logical function h5exist(filename, dname)
character(*), intent(in) :: filename, dname
end function h5exist


module subroutine lt0write_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(in) :: value
integer, intent(out), optional :: ierr
end subroutine lt0write_r32

module subroutine lt0write_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(in) :: value
integer, intent(out), optional :: ierr
end subroutine lt0write_r64

module subroutine lt0write_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(in) :: value
integer, intent(out), optional :: ierr
end subroutine lt0write_i32

module subroutine lt0write_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(in) :: value
integer, intent(out), optional :: ierr
end subroutine lt0write_i64

module subroutine lt0write_char(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
character(*), intent(in) :: value
integer, intent(out), optional :: ierr
end subroutine lt0write_char


module subroutine lt1write_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(in) :: value(:)
integer, intent(out), optional :: ierr
end subroutine lt1write_r32

module subroutine lt1write_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(in) :: value(:)
integer, intent(out), optional :: ierr
end subroutine lt1write_r64

module subroutine lt1write_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(in) :: value(:)
integer, intent(out), optional :: ierr
end subroutine lt1write_i32

module subroutine lt1write_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(in) :: value(:)
integer, intent(out), optional :: ierr
end subroutine lt1write_i64


module subroutine lt2write_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(in) :: value(:,:)
integer, intent(out), optional :: ierr
end subroutine lt2write_r32

module subroutine lt2write_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(in) :: value(:,:)
integer, intent(out), optional :: ierr
end subroutine lt2write_r64

module subroutine lt2write_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(in) :: value(:,:)
integer, intent(out), optional :: ierr
end subroutine lt2write_i32

module subroutine lt2write_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(in) :: value(:,:)
integer, intent(out), optional :: ierr
end subroutine lt2write_i64


module subroutine lt3write_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(in) :: value(:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt3write_r32

module subroutine lt3write_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(in) :: value(:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt3write_r64

module subroutine lt3write_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(in) :: value(:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt3write_i32

module subroutine lt3write_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(in) :: value(:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt3write_i64


module subroutine lt4write_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(in) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt4write_r32

module subroutine lt4write_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(in) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt4write_r64

module subroutine lt4write_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(in) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt4write_i32

module subroutine lt4write_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(in) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt4write_i64


module subroutine lt5write_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(in) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt5write_r32

module subroutine lt5write_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(in) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt5write_r64

module subroutine lt5write_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(in) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt5write_i32

module subroutine lt5write_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(in) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt5write_i64


module subroutine lt6write_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(in) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt6write_r32

module subroutine lt6write_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(in) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt6write_r64

module subroutine lt6write_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(in) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt6write_i32

module subroutine lt6write_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(in) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt6write_i64


module subroutine lt7write_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt7write_r32

module subroutine lt7write_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt7write_r64

module subroutine lt7write_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt7write_i32

module subroutine lt7write_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt7write_i64

end interface

interface !< reader_lt.f90
module subroutine lt0read_r32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real32), intent(out) :: value
integer, intent(out), optional :: ierr
end subroutine lt0read_r32

module subroutine lt0read_r64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
real(real64), intent(out) :: value
integer, intent(out), optional :: ierr
end subroutine lt0read_r64

module subroutine lt0read_i32(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int32), intent(out) :: value
integer, intent(out), optional :: ierr
end subroutine lt0read_i32

module subroutine lt0read_i64(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
integer(int64), intent(out) :: value
integer, intent(out), optional :: ierr
end subroutine lt0read_i64

module subroutine lt0read_char(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
character(*), intent(out) :: value
integer, intent(out), optional :: ierr
end subroutine lt0read_char



!! NOTE: intent(inout) for non-scalar read is avoid reallocation and segfault with allocatable arrays in user programs,
!! even when the array was already allocated before the h5fortran interface was called.
module subroutine lt1read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:)
integer, intent(out), optional :: ierr
end subroutine lt1read

module subroutine lt2read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:)
integer, intent(out), optional :: ierr
end subroutine lt2read

module subroutine lt3read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt3read

module subroutine lt4read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt4read

module subroutine lt5read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt5read

module subroutine lt6read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt6read

module subroutine lt7read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt7read
end interface

interface !< writer.f90
module subroutine hdf_write_scalar_r32(self, dname, value, ierr, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_scalar_r32

module subroutine hdf_write_scalar_r64(self, dname, value, ierr, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_scalar_r64

module subroutine hdf_write_scalar_i32(self, dname, value, ierr, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_scalar_i32

module subroutine hdf_write_scalar_i64(self, dname, value, ierr, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int64), intent(in) :: value
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_scalar_i64

module subroutine hdf_write_scalar_char(self, dname, value, ierr, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
character(*), intent(in) :: value
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_scalar_char


module subroutine hdf_write_1d_r32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:)
integer, intent(in), optional :: chunk_size(1)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_1d_r32

module subroutine hdf_write_1d_r64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:)
integer, intent(in), optional :: chunk_size(1)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_1d_r64

module subroutine hdf_write_1d_i32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:)
integer, intent(in), optional :: chunk_size(1)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_1d_i32

module subroutine hdf_write_1d_i64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int64), intent(in) :: value(:)
integer, intent(in), optional :: chunk_size(1)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_1d_i64


module subroutine hdf_write_2d_r32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(2)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_2d_r32

module subroutine hdf_write_2d_r64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(2)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_2d_r64

module subroutine hdf_write_2d_i32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(2)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_2d_i32

module subroutine hdf_write_2d_i64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int64), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(2)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_2d_i64



module subroutine hdf_write_3d_r32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(3)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_3d_r32

module subroutine hdf_write_3d_r64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(3)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_3d_r64

module subroutine hdf_write_3d_i32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(3)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_3d_i32

module subroutine hdf_write_3d_i64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int64), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(3)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_3d_i64


module subroutine hdf_write_4d_r32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(4)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_4d_r32

module subroutine hdf_write_4d_r64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(4)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_4d_r64

module subroutine hdf_write_4d_i32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(4)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_4d_i32

module subroutine hdf_write_4d_i64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int64), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(4)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_4d_i64

module subroutine hdf_write_5d_r32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(5)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_5d_r32

module subroutine hdf_write_5d_r64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(5)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_5d_r64

module subroutine hdf_write_5d_i32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(5)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_5d_i32

module subroutine hdf_write_5d_i64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int64), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(5)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_5d_i64

module subroutine hdf_write_6d_r32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(6)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_6d_r32

module subroutine hdf_write_6d_r64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(6)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_6d_r64

module subroutine hdf_write_6d_i32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(6)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_6d_i32

module subroutine hdf_write_6d_i64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int64), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(6)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_6d_i64

module subroutine hdf_write_7d_r32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(7)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_7d_r32

module subroutine hdf_write_7d_r64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(7)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_7d_r64

module subroutine hdf_write_7d_i32(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(7)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_7d_i32

module subroutine hdf_write_7d_i64(self,dname,value, ierr, chunk_size, istart, iend, stride, compact)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int64), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(7)
integer, intent(in), optional, dimension(:) :: istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(out), optional :: ierr
end subroutine hdf_write_7d_i64
end interface

interface !< read.f90
module integer function hdf_get_ndims(self, dname) result (drank)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_get_ndims

module subroutine hdf_get_shape(self, dname, dims, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(out), allocatable :: dims(:)
integer, intent(out), optional :: ierr
end subroutine hdf_get_shape

module integer function hdf_get_layout(self, dname) result(layout)
!! H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_VIRTUAL_F, H5D_COMPACT_F
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_get_layout

module subroutine hdf_get_chunk(self, dname, chunk_size)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(hsize_t), intent(out) :: chunk_size(:)
end subroutine hdf_get_chunk

module logical function hdf_check_exist(self, dname) result(exists)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_check_exist
end interface

interface !< reader.f90

module subroutine hdf_read_scalar_char(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
character(*), intent(inout)      :: value
!! intent(inout) for character
integer, intent(out), optional :: ierr
end subroutine hdf_read_scalar_char

module subroutine hdf_read_scalar_r32(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
real(real32), intent(out)        :: value
!! intent(inout) for character
integer, intent(out), optional :: ierr
end subroutine hdf_read_scalar_r32

module subroutine hdf_read_scalar_r64(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
real(real64), intent(out)        :: value
integer, intent(out), optional :: ierr
end subroutine hdf_read_scalar_r64

module subroutine hdf_read_scalar_i32(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out)        :: value
integer, intent(out), optional :: ierr
end subroutine hdf_read_scalar_i32

module subroutine hdf_read_scalar_i64(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int64), intent(out)        :: value
integer, intent(out), optional :: ierr
end subroutine hdf_read_scalar_i64

module subroutine hdf_read_1d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_1d

module subroutine hdf_read_2d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_2d

module subroutine hdf_read_3d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_3d

module subroutine hdf_read_4d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_4d

module subroutine hdf_read_5d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_5d

module subroutine hdf_read_6d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_6d

module subroutine hdf_read_7d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_7d
end interface


interface  !< attributes.f90

module subroutine readattr_char(self, dname, attr, attrval, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
character(*), intent(inout) :: attrval
!! intent(inout) for character
integer, intent(out), optional :: ierr
end subroutine readattr_char

module subroutine readattr_num(self, dname, attr, attrval, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
class(*), intent(out) :: attrval(:)
integer, intent(out), optional :: ierr
end subroutine readattr_num

module subroutine writeattr_char(self, dname, attr, attrval, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
character(*), intent(in) :: attrval
integer, intent(out), optional :: ierr
end subroutine writeattr_char

module subroutine writeattr_num(self, dname, attr, attrval, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
class(*), intent(in) :: attrval(:)
integer, intent(out), optional :: ierr
end subroutine writeattr_num

module subroutine writeattr_char_lt(filename, dname, attr, attrval, ierr)
character(*), intent(in) :: filename
character(*), intent(in) :: dname, attr
character(*), intent(in) :: attrval
integer, intent(out), optional :: ierr
end subroutine writeattr_char_lt

module subroutine writeattr_num_lt(filename, dname, attr, attrval, ierr)
character(*), intent(in) :: filename
character(*), intent(in) :: dname, attr
class(*), intent(in) :: attrval(:)
integer, intent(out), optional :: ierr
end subroutine writeattr_num_lt

module subroutine readattr_char_lt(filename, dname, attr, attrval, ierr)
character(*), intent(in) :: filename
character(*), intent(in) :: dname, attr
character(*), intent(inout) :: attrval
!! intent(inout) for character
integer, intent(out), optional :: ierr
end subroutine readattr_char_lt

module subroutine readattr_num_lt(filename, dname, attr, attrval, ierr)
character(*), intent(in) :: filename
character(*), intent(in) :: dname, attr
class(*), intent(out) :: attrval(:)
integer, intent(out), optional :: ierr
end subroutine readattr_num_lt

end interface

contains


subroutine hdf_initialize(self,filename,ierr, status,action,comp_lvl,verbose,debug)
!! Opens hdf5 file

class(hdf5_file), intent(inout)    :: self
character(*), intent(in) :: filename
integer, intent(out), optional :: ierr  !< 0 if OK
character(*), intent(in), optional :: status  !< DEPRECATED
character(*), intent(in), optional :: action !< r, r+, rw, w, a
integer, intent(in), optional      :: comp_lvl  !< 0: no compression. 1-9: ZLIB compression, higher is more compressior
logical, intent(in), optional      :: verbose, debug

character(:), allocatable :: laction
integer :: ier

if(self%is_open) then
  write(stderr,*) 'h5fortran:open: file handle already open: '//self%filename
  return
endif

self%filename = filename

if (present(comp_lvl)) self%comp_lvl = comp_lvl
if (present(verbose)) self%verbose = verbose
if (present(debug)) self%debug = debug

!> Initialize FORTRAN interface.
call h5open_f(ier)
if (ier /= 0) error stop 'h5fortran:open: HDF5 library initialize'

!> get library version
call h5get_libversion_f(self%libversion(1), self%libversion(2), self%libversion(3), ier)
if (self%debug) print '(a,i0,a1,i0,a1,i0)', 'HDF5 version: ',self%libversion(1),'.',self%libversion(2),'.',self%libversion(3)
if (check(ier, 'ERROR:h5fortran: HDF5 library get version')) error stop

if(self%verbose) then
  call h5eset_auto_f(1, ier)
else
  call h5eset_auto_f(0, ier)
endif
if(present(ierr)) ierr = ier
if (check(ier, 'ERROR:h5fortran: HDF5 library set traceback')) then
  if (present(ierr)) return
  error stop
endif

if(present(status)) write(stderr,*) "h5fortran:open: WARNING: status argument is deprecated. use action instead."

laction = 'rw'
if(present(action)) laction = action

select case(laction)
case('r')
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, self%lid,ier)
case('r+')
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
  error stop 'h5fortran: Unsupported action: ' // laction
end select

if (present(ierr)) ierr = ier
if (check(ier, filename)) then
  if (present(ierr)) return
  error stop
endif

self%is_open = .true.

end subroutine hdf_initialize


subroutine hdf_finalize(self, ierr, close_hdf5_interface)
!! This must be called on each HDF5 file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! We don't reference count because applications might also invoke HDF5
!! directly.
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

class(hdf5_file), intent(inout) :: self
logical, intent(in), optional :: close_hdf5_interface
integer, intent(out), optional :: ierr
integer :: ier

if (.not. self%is_open) then
  write(stderr,*) 'WARNING:h5fortran:close: file handle is already closed: '// self%filename
  return
endif

!> close hdf5 file
call h5fclose_f(self%lid, ier)
if (present(ierr)) ierr = ier
if (check(ier, 'ERROR:h5fortran:close: HDF5 file close: ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

if (present(close_hdf5_interface)) then
  if (close_hdf5_interface) then
    call h5close_f(ier)
    if (present(ierr)) ierr = ier
    if (check(ier, 'ERROR:h5fortran: HDF5 library close')) then
      if (present(ierr)) return
      error stop
    endif
  endif
endif
!> sentinel lid
self%lid = 0

self%is_open = .false.

end subroutine hdf_finalize


subroutine destructor(self)
!! Close file and handle if user forgets to do so

type(hdf5_file), intent(inout) :: self

if (.not. self%is_open) return

print *, "auto-closing " // self%filename

call self%close()

end subroutine destructor


subroutine hdf_flush(self, ierr)
!! request operating system flush data to disk.
!! The operating system can do this when it desires, which might be a while.
class(hdf5_file), intent(in) :: self
integer, intent(out), optional :: ierr
integer :: ier

call h5fflush_f(self%lid, H5F_SCOPE_GLOBAL_F, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: HDF5 flush ' // self%filename) .and. .not.present(ierr)) error stop

end subroutine hdf_flush


subroutine hdf5_close(ierr)
!! this subroutine will close ALL existing file handles
!! only call it at end of your program
!! "Flushes all data to disk, closes all open identifiers, and cleans up memory."
!! "Should be called by all HDF5 Fortran programs"

integer, intent(out), optional :: ierr
integer :: ier

call h5close_f(ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: HDF5 library close') .and. .not.present(ierr)) error stop

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


subroutine write_group(self, gname, ierr)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: gname    !< relative path to group
integer, intent(out), optional :: ierr

integer(HID_T)  :: gid
integer :: ier

integer :: sp, ep, sl
logical :: gexist

if(.not.self%is_open) error stop 'h5fortran:write_group: file handle is not open'

sl = len(gname)
sp = 1
ep = 0

do
  ep = index(gname(sp+1:sl), "/")

  ! no subgroup found
  if (ep == 0) exit

  ! check subgroup exists
  sp = sp + ep
  call h5lexists_f(self%lid, gname(1:sp-1), gexist, ier)
  if (present(ierr)) ierr = ier
  if (check(ier, self%filename, gname)) then
    if (present(ierr)) return
    error stop
  endif

  if(.not.gexist) then
    call h5gcreate_f(self%lid, gname(1:sp-1), gid, ier)
    if (present(ierr)) ierr = ier
    if (check(ier, self%filename, gname)) then
      if (present(ierr)) return
      error stop
    endif

    call h5gclose_f(gid, ier)
    if (present(ierr)) ierr = ier
    if (check(ier, self%filename, gname)) then
      if (present(ierr)) return
      error stop
    endif
  endif
end do

end subroutine write_group


logical function check(ierr, filename, dname)
integer, intent(in) :: ierr
character(*), intent(in), optional :: filename, dname

character(:), allocatable :: fn, dn

check = .false.
if (ierr==0) return

check = .true.
fn = ""
dn = ""
if (present(filename)) fn = filename
if (present(dname)) dn = dname

write(stderr,*) 'h5fortran:ERROR: ' // fn // ':' // dn // ' error code ', ierr

end function check


subroutine hdf_wrapup(dset_id, space_id)

integer(HID_T), intent(in) :: dset_id
integer(HID_T), intent(in), optional :: space_id

integer :: ierr

ierr = 0

if(present(space_id)) then
  if(space_id /= 0) call h5sclose_f(space_id, ierr)
  if (ierr /= 0) error stop 'h5sclose dataspace'
endif

if(dset_id /= 0) then
  call h5dclose_f(dset_id, ierr)
  if (ierr /= 0) error stop 'h5dclose dataset'
endif

end subroutine hdf_wrapup


subroutine hdf_get_slice(self, dname, did, sid, mem_sid, i0, i1, i2)
!! setup array slices for read and write
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(inout) :: did  !< inout for sentinel value
integer(hid_t), intent(out) :: sid, mem_sid
class(*), intent(in), dimension(:) :: i0, i1
class(*), intent(in), optional, dimension(:) :: i2

integer(hsize_t), dimension(size(i0)) :: istart, iend, stride, mem_dims
integer :: ierr

if (size(i0) /= size(i1)) error stop "istart and iend must have equal length"
if (present(i2)) then
  if (size(i0) /= size(i2)) error stop "istride must be same length as istart and iend"
endif

if(.not.self%is_open) error stop 'h5fortran:slice: file handle is not open'

!! istart
select type (i0)
type is (integer(int32))
  istart = int(i0, int64)
type is (integer(hsize_t))
  istart = i0
class default
  error stop 'ERROR:h5fortran:slice: wrong type for istart: ' // dname // ' in ' // self%filename
end select

!! iend

select type (i1)
type is (integer(int32))
  iend = int(i1, int64)
type is (integer(hsize_t))
  iend = i1
class default
  error stop 'ERROR:h5fortran:slice: wrong type for iend: ' // dname // ' in ' // self%filename
end select

!! stride

if (present(i2)) then
  select type (i2)
  type is (integer(int32))
    stride = int(i2, int64)
  type is (integer(hsize_t))
    stride = i2
  class default
    error stop 'ERROR:h5fortran:slice: wrong type for stride: ' // dname // ' in ' // self%filename
  end select
  if(self%debug) print *,'DEBUG: user-stride:',stride
else
  stride = 1
  if(self%debug) print *, 'DEBUG: auto-stride',stride
endif

!! compensate for 0-based hyperslab vs. 1-based Fortran
istart = istart - 1

mem_dims = iend - istart

!> some callers have already opened the dataset. 0 is a sentinel saying not opened yet.
if (did == 0) then
  call h5dopen_f(self%lid, dname, did, ierr)
  if(ierr /= 0) error stop 'h5fortran:get_slice:H5Dopen: ' // dname // ' ' // self%filename
endif
call h5dget_space_f(did, sid, ierr)
if(ierr /= 0) error stop 'h5fortran:get_slice could not get dataset'
call h5sselect_hyperslab_f(sid, H5S_SELECT_SET_F, istart, mem_dims, ierr, stride=stride)
if(ierr /= 0) error stop 'h5fortran:get_slice could not assign hyperslab'
call h5screate_simple_f(size(mem_dims), mem_dims, mem_sid, ierr)
if(ierr /= 0) error stop 'h5fortran:get_slice could not create dataspace'

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

if(.not.self%is_open) error stop 'h5fortran:rank_check: file handle is not open'

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
if (ierr/=0) error stop 'h5fortran:shape_check: get_dataset_info ' // dname // ' read ' // self%filename


if(any(int(dims, int64) /= ddims)) then
  write(stderr,*) 'h5fortran:shape_check: shape mismatch ' // dname // ' = ',ddims,'  variable shape =', dims
  error stop
endif

end subroutine hdf_shape_check


end module h5fortran
