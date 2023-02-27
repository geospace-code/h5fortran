module h5fortran
!! HDF5 object-oriented polymorphic interface

use, intrinsic :: iso_c_binding, only : c_ptr, c_loc
use, intrinsic :: iso_fortran_env, only : real32, real64, int64, int32, stderr=>error_unit

use hdf5, only : HID_T, SIZE_T, HSIZE_T, &
H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE, &
H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F, &
H5P_DEFAULT_F

implicit none

private


!> main type
type :: hdf5_file

character(:), allocatable :: filename
integer(HID_T) :: file_id = 0  !< sentinel value to avoid uninitialized variable lint

logical :: debug = .false.
logical :: fletcher32 = .false.
logical :: shuffle = .false.

integer :: comp_lvl = 0
!! compression level (1-9)  0: disable compression

logical :: use_mpi = .false. !< MPI or serial HDF5


contains
!> define methods (procedures) that don't need generic procedure
procedure, public :: open => h5open
procedure, public :: close => h5close
procedure, public :: write_group => create_group !< legacy
procedure, public :: create_group
procedure, public :: create => hdf_create_user
procedure, public :: flush => hdf_flush
procedure, public :: filesize => hdf_filesize
procedure, public :: ndim => hdf_get_ndim
procedure, public :: ndims => hdf_get_ndim !< legacy
procedure, public :: shape => hdf_get_shape
procedure, public :: layout => hdf_get_layout
procedure, public :: chunks => hdf_get_chunk
procedure, public :: class => get_class
procedure, public :: dtype => get_native_dtype
procedure, public :: deflate => get_deflate
procedure, public :: exist => hdf_check_exist
procedure, public :: is_contig => hdf_is_contig
procedure, public :: is_chunked => hdf_is_chunked
procedure, public :: is_compact => hdf_is_compact
procedure, public :: get_strpad
procedure, public :: softlink => create_softlink
procedure, public :: is_open
procedure, public :: delete_attr => attr_delete
procedure, public :: exist_attr => attr_exist
!! procedures without mapping

!> below are procedure that need generic mapping (type or rank agnostic)

generic, public :: write => h5write_scalar, h5write_1d, h5write_2d, h5write_3d, h5write_4d, h5write_5d, h5write_6d, h5write_7d
procedure, private :: h5write_scalar, h5write_1d, h5write_2d, h5write_3d, h5write_4d, h5write_5d, h5write_6d, h5write_7d

generic, public :: read => h5read_scalar, h5read_1d, h5read_2d, h5read_3d, h5read_4d, h5read_5d, h5read_6d, h5read_7d
procedure, private :: h5read_scalar, h5read_1d, h5read_2d, h5read_3d, h5read_4d, h5read_5d, h5read_6d, h5read_7d

!> attributes
generic, public :: writeattr => writeattr_scalar, writeattr_1d, writeattr_2d, writeattr_3d, writeattr_4d, writeattr_5d, &
  writeattr_6d, writeattr_7d
procedure, private :: writeattr_scalar, writeattr_1d, writeattr_2d, writeattr_3d, writeattr_4d, writeattr_5d, &
  writeattr_6d, writeattr_7d

generic, public :: readattr => readattr_scalar, readattr_1d, readattr_2d, readattr_3d, readattr_4d, readattr_5d, &
readattr_6d, readattr_7d
procedure, private :: readattr_scalar, readattr_1d, readattr_2d, readattr_3d, readattr_4d, readattr_5d, &
readattr_6d, readattr_7d

!> flush file to disk and close file if user forgets to do so.
final :: destructor

end type hdf5_file


interface h5write
procedure lt0write, lt1write, lt2write, lt3write, lt4write, lt5write, lt6write, lt7write
end interface

interface h5read
procedure lt0read, lt1read, lt2read, lt3read, lt4read, lt5read, lt6read, lt7read
end interface

interface h5write_attr
procedure lt0writeattr, lt1writeattr
end interface

interface h5read_attr
procedure lt0readattr, lt1readattr
end interface

interface read_char
procedure read_char0, read_char1, read_char2, read_char3, read_char4, read_char5, read_char6, read_char7
end interface

public :: hdf5_is_initialized
public :: hdf5_file, hdf5_close, h5write, h5read, h5exist, is_hdf5, h5write_attr, h5read_attr, hdf5version
!! for users
public :: hdf_shape_check, hdf_rank_check, hdf_get_slice, id2name, get_obj_class, estop
!! for submodules only
public :: HSIZE_T, HID_T, H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE
public :: H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F
!! HDF5 types for end users


!> Submodules

interface !< write.f90

module subroutine hdf_create_user(self, dname, dtype, dset_dims, mem_dims, chunk_size, compact, charlen, fill_value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer, dimension(:), intent(in) :: dset_dims
integer, dimension(:), intent(in), optional :: mem_dims
integer, intent(in), dimension(:), optional :: chunk_size  !< (:) instead of size(dims) due to intel fortran quirk
logical, intent(in), optional :: compact
integer, intent(in), optional :: charlen
class(*), intent(in), optional :: fill_value
end subroutine

module subroutine hdf_create(self, dname, dtype, mem_dims, dset_dims, filespace_id, dset_id, dtype_id, &
  chunk_size, istart, iend, stride, compact, charlen, fill_value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: mem_dims, dset_dims
integer(HID_T), intent(out) :: filespace_id, dset_id, dtype_id
integer, intent(in), dimension(:), optional :: chunk_size, istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(in), optional :: charlen !< length of character scalar
class(*), intent(in), optional :: fill_value
end subroutine

module subroutine create_group(self, group_path)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: group_path   !< full path to group
end subroutine

module subroutine create_softlink(self, tgt, link)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: tgt, &  !< target path to link
                            link  !< soft link path to create
end subroutine

module subroutine hdf_flush(self)
!! request operating system flush data to disk.
!! The operating system can do this when it desires, which might be a while.
class(hdf5_file), intent(in) :: self
end subroutine

end interface

interface !< writer_lt.f90

module subroutine lt0write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A
end subroutine

module subroutine lt1write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A(:)
end subroutine

module subroutine lt2write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A(:,:)
end subroutine

module subroutine lt3write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A(:,:,:)
end subroutine

module subroutine lt4write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A(:,:,:,:)
end subroutine

module subroutine lt5write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A(:,:,:,:,:)
end subroutine

module subroutine lt6write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A(:,:,:,:,:,:)
end subroutine

module subroutine lt7write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A(:,:,:,:,:,:,:)
end subroutine

end interface


interface !< reader_lt.f90

module logical function h5exist(filename, dname, debug)
character(*), intent(in) :: filename, dname
logical, intent(in), optional :: debug
end function

module subroutine lt0read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: A
end subroutine

module subroutine lt1read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: A(:)
end subroutine

module subroutine lt2read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: A(:,:)
end subroutine

module subroutine lt3read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: A(:,:,:)
end subroutine

module subroutine lt4read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: A(:,:,:,:)
end subroutine

module subroutine lt5read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: A(:,:,:,:,:)
end subroutine

module subroutine lt6read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: A(:,:,:,:,:,:)
end subroutine

module subroutine lt7read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(inout) :: A(:,:,:,:,:,:,:)
end subroutine
end interface


interface !< writer.f90

module subroutine h5write_scalar(self, dname, A, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_1d(self, dname, A, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in), dimension(:) :: A
integer, intent(in), dimension(1), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_2d(self, dname, A, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in), dimension(:,:) :: A
integer, intent(in), dimension(2), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_3d(self, dname, A, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in), dimension(:,:,:) :: A
integer, intent(in), dimension(3), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_4d(self, dname, A, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in), dimension(:,:,:,:) :: A
integer, intent(in), dimension(4), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_5d(self, dname, A, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in), dimension(:,:,:,:,:) :: A
integer, intent(in), dimension(5), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_6d(self, dname, A, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in), dimension(:,:,:,:,:,:) :: A
integer, intent(in), dimension(6), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_7d(self, dname, A, chunk_size, istart, iend, stride, compact, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in), dimension(:,:,:,:,:,:,:) :: A
integer, intent(in), dimension(7), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

end interface


interface !< read.f90

module integer function get_class(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module subroutine get_obj_class(self, obj_name, obj_id, class, size_bytes, pad_type)
!! get the object class (integer, float, string, ...)
!! {H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F}
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
integer(HID_T), intent(in) :: obj_id
integer, intent(out) :: class
integer(SIZE_T), intent(out), optional :: size_bytes
integer, intent(out), optional :: pad_type
end subroutine

module integer(hid_t) function get_native_dtype(self, dname, obj_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(hid_t), intent(in), optional :: obj_id
end function

module integer function hdf_get_ndim(self, dname) result (drank)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module subroutine hdf_get_shape(self, obj_name, dims, attr_name)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
integer(HSIZE_T), intent(out), allocatable :: dims(:)
character(*), intent(in), optional :: attr_name
end subroutine

module integer function get_strpad(self, dset_name)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dset_name
end function

module logical function get_deflate(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module integer function hdf_get_layout(self, dname) result(layout)
!! H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_VIRTUAL_F, H5D_COMPACT_F
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module subroutine hdf_get_chunk(self, dname, chunk_size)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(out) :: chunk_size(:)
end subroutine

module logical function hdf_check_exist(self, obj_name)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
end function

end interface


interface !< reader.f90

module subroutine read_char0(self, obj_name, A, obj_id, mem_space_id, file_space_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
character(*), intent(inout) :: A
integer(HID_T), intent(in) :: obj_id, mem_space_id, file_space_id
end subroutine

module subroutine read_char1(self, obj_name, A, obj_id, mem_space_id, file_space_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
character(*), intent(inout), dimension(:) :: A
integer(HID_T), intent(in) :: obj_id, mem_space_id, file_space_id
end subroutine

module subroutine read_char2(self, obj_name, A, obj_id, mem_space_id, file_space_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
character(*), intent(inout), dimension(:,:) :: A
integer(HID_T), intent(in) :: obj_id, mem_space_id, file_space_id
end subroutine

module subroutine read_char3(self, obj_name, A, obj_id, mem_space_id, file_space_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
character(*), intent(inout), dimension(:,:,:) :: A
integer(HID_T), intent(in) :: obj_id, mem_space_id, file_space_id
end subroutine

module subroutine read_char4(self, obj_name, A, obj_id, mem_space_id, file_space_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
character(*), intent(inout), dimension(:,:,:,:) :: A
integer(HID_T), intent(in) :: obj_id, mem_space_id, file_space_id
end subroutine

module subroutine read_char5(self, obj_name, A, obj_id, mem_space_id, file_space_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
character(*), intent(inout), dimension(:,:,:,:,:) :: A
integer(HID_T), intent(in) :: obj_id, mem_space_id, file_space_id
end subroutine

module subroutine read_char6(self, obj_name, A, obj_id, mem_space_id, file_space_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
character(*), intent(inout), dimension(:,:,:,:,:,:) :: A
integer(HID_T), intent(in) :: obj_id, mem_space_id, file_space_id
end subroutine

module subroutine read_char7(self, obj_name, A, obj_id, mem_space_id, file_space_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
character(*), intent(inout), dimension(:,:,:,:,:,:,:) :: A
integer(HID_T), intent(in) :: obj_id, mem_space_id, file_space_id
end subroutine
module subroutine h5read_scalar(self, dname, A)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout)          :: A
end subroutine

module subroutine h5read_1d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout), dimension(:) :: A
integer, intent(in), dimension(1), optional :: istart, iend, stride
end subroutine

module subroutine h5read_2d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout), dimension(:,:) :: A
integer, intent(in), dimension(2), optional :: istart, iend, stride
end subroutine

module subroutine h5read_3d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout), dimension(:,:,:) :: A
integer, intent(in), dimension(3), optional :: istart, iend, stride
end subroutine

module subroutine h5read_4d(self, dname, A,  istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout), dimension(:,:,:,:) :: A
integer, intent(in), dimension(4), optional :: istart, iend, stride
end subroutine

module subroutine h5read_5d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout), dimension(:,:,:,:,:) :: A
integer, intent(in), dimension(5), optional :: istart, iend, stride
end subroutine

module subroutine h5read_6d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout), dimension(:,:,:,:,:,:) :: A
integer, intent(in), dimension(6), optional :: istart, iend, stride
end subroutine

module subroutine h5read_7d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout), dimension(:,:,:,:,:,:,:) :: A
integer, intent(in), dimension(7), optional :: istart, iend, stride
end subroutine

end interface


interface  !< attributes.f90

module subroutine readattr_scalar(self, obj_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
class(*), intent(inout) :: A
end subroutine

module subroutine readattr_1d(self, obj_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
class(*), intent(inout) :: A(:)
end subroutine

module subroutine readattr_2d(self, obj_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
class(*), intent(inout) :: A(:,:)
end subroutine

module subroutine readattr_3d(self, obj_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
class(*), intent(inout) :: A(:,:,:)
end subroutine

module subroutine readattr_4d(self, obj_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
class(*), intent(inout) :: A(:,:,:,:)
end subroutine

module subroutine readattr_5d(self, obj_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
class(*), intent(inout) :: A(:,:,:,:,:)
end subroutine

module subroutine readattr_6d(self, obj_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
class(*), intent(inout) :: A(:,:,:,:,:,:)
end subroutine

module subroutine readattr_7d(self, obj_name, attr_name, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
class(*), intent(inout) :: A(:,:,:,:,:,:,:)
end subroutine


module subroutine writeattr_scalar(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A
end subroutine

module subroutine writeattr_1d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:)
end subroutine

module subroutine writeattr_2d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:)
end subroutine

module subroutine writeattr_3d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:)
end subroutine

module subroutine writeattr_4d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:,:)
end subroutine

module subroutine writeattr_5d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:,:,:)
end subroutine

module subroutine writeattr_6d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:,:,:,:)
end subroutine

module subroutine writeattr_7d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:,:,:,:,:)
end subroutine


module subroutine lt0writeattr(filename, obj_name, attr, A)
character(*), intent(in) :: filename
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A
end subroutine

module subroutine lt1writeattr(filename, obj_name, attr, A)
character(*), intent(in) :: filename
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:)
end subroutine

module subroutine lt0readattr(filename, obj_name, attr, A)
character(*), intent(in) :: filename
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A
end subroutine

module subroutine lt1readattr(filename, obj_name, attr, A)
character(*), intent(in) :: filename
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A(:)
end subroutine

module subroutine attr_delete(self, obj_name, attr_name)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
end subroutine

module logical function attr_exist(self, obj_name, attr_name)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
end function

end interface


interface !< utils.f90

module function id2name(id)
!! get name of object with given id
integer(HID_T), intent(in) :: id
character(:), allocatable :: id2name
end function

module subroutine h5open(self, filename, action, comp_lvl, shuffle, fletcher32, debug)
!! open/create file
!!
!! PARAMETERS:
!! ----------
!! filename
!! action: "r", "r+", "rw", "w", "a"
!! comp_lvl; < 0: no compression. 1-9: ZLIB compression, higher is more compression

class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: filename
character(*), intent(in), optional :: action
integer, intent(in), optional :: comp_lvl
logical, intent(in), optional :: shuffle
logical, intent(in), optional :: fletcher32
logical, intent(in), optional :: debug
end subroutine

module subroutine h5close(self, close_hdf5_interface)
!! This must be called on each HDF5 file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! We don't reference count because applications might also invoke HDF5
!! directly.
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

class(hdf5_file), intent(inout) :: self
logical, intent(in), optional :: close_hdf5_interface
end subroutine

module logical function is_open(self)
!! check if file handle is open
class(hdf5_file), intent(in) :: self
end function

module logical function hdf5_is_initialized()
!! check if HDF5 library is initialized from any object (including outside h5fortran)
end function

module subroutine destructor(self)
!! Close file and handle if user forgets to do so
type(hdf5_file), intent(inout) :: self
end subroutine

module integer function hdf5version() result(v)
!! tell HDF5 library version (major, minor, release)
dimension :: v(3)
end function

module subroutine hdf5_close()
!! this subroutine will close ALL existing file handles
!! only call it at end of your program
!! "Flushes all data to disk, closes all open identifiers, and cleans up memory."
!! "Should be called by all HDF5 Fortran programs
end subroutine

module logical function hdf_is_contig(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module logical function hdf_is_compact(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module logical function hdf_is_chunked(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module logical function is_hdf5(filename)
!! is this file HDF5?
character(*), intent(in) :: filename
end function

module subroutine hdf_get_slice(mem_dims, dset_id, file_space_id, mem_space_id, istart, iend, stride, dset_dims)
!! setup array slices for read and write
integer(HSIZE_T), dimension(:), intent(in) :: mem_dims
integer(HID_T), intent(inout) :: dset_id  !< inout for sentinel value
integer(HID_T), intent(in) :: file_space_id
integer(HID_T), intent(out) :: mem_space_id
integer, intent(in), dimension(:) :: istart
integer, intent(in), dimension(size(istart)) :: iend
integer, intent(in), dimension(size(istart)), optional :: stride
integer(HSIZE_T), intent(in), dimension(size(istart)), optional :: dset_dims
end subroutine

module subroutine hdf_rank_check(self, obj_name, file_space_id, mrank, is_scalar)
!! check for matching rank, else bad reads can occur--doesn't always crash without this check
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
integer(HID_T), intent(in) :: file_space_id
integer, intent(in) :: mrank
logical, intent(out), optional :: is_scalar
end subroutine

module subroutine hdf_shape_check(self, dname, file_space_id, dims)
!! check for matching size, else bad reads can occur.
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: file_space_id
integer(HSIZE_T), intent(in) :: dims(:)
end subroutine

module integer(HSIZE_T) function hdf_filesize(self)
!! returns the size of the HDF5 file in bytes
class(hdf5_file), intent(in) :: self
end function

module pure subroutine estop(ier, id, filename, obj_name, attr_name)
integer, intent(in) :: ier
character(*), intent(in) :: id, filename
character(*), intent(in), optional :: obj_name, attr_name
end subroutine

end interface


end module h5fortran
