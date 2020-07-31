module h5fortran
!! HDF5 object-oriented polymorphic interface
use, intrinsic :: iso_c_binding, only : c_ptr, c_loc
use, intrinsic :: iso_fortran_env, only : real32, real64, int64, int32, stderr=>error_unit
use hdf5, only : HID_T, SIZE_T, HSIZE_T, H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
  H5S_ALL_F, H5S_SELECT_SET_F, &
  H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5F_SCOPE_GLOBAL_F, &
  h5open_f, h5close_f, &
  h5dopen_f, h5dclose_f, h5dget_space_f, &
  h5gcreate_f, h5gclose_f, &
  h5fopen_f, h5fcreate_f, h5fclose_f, h5fis_hdf5_f, &
  h5lexists_f, &
  h5sclose_f, h5sselect_hyperslab_f, h5screate_simple_f, &
  h5get_libversion_f, h5eset_auto_f, h5fflush_f
use h5lt, only : h5ltget_dataset_ndims_f, h5ltget_dataset_info_f

use pathlib, only : unlink, get_tempdir, is_absolute_path
use string_utils, only : toLower, strip_trailing_null, truncate_string_null

implicit none (type, external)
private
public :: hdf5_file, hdf5_close, toLower, h5write, h5read, h5exist, is_hdf5, &
  check, hdf_shape_check, hdf_get_slice, hdf_wrapup, hsize_t, strip_trailing_null, truncate_string_null

!> Workaround for Intel 19.1 / 2020 bug with /stand:f18
!> error #6410: This name has not been declared as an array or a function.   [RANK]
!> GCC 10.2.0 generates spurious Wsurprising from having this here.
intrinsic :: rank

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
logical :: is_scratch = .false.
!! will be auto-deleted on close
integer :: libversion(3)  !< major, minor, rel


contains
!> define methods (procedures) that don't need generic procedure
procedure, public :: initialize => hdf_initialize, finalize => hdf_finalize, &
  write_group, &
  open => hdf_open_group, close => hdf_close_group, flush => hdf_flush, &
  ndims => hdf_get_ndims, &
  shape => hdf_get_shape, layout => hdf_get_layout, chunks => hdf_get_chunk, &
  exist => hdf_check_exist, exists => hdf_check_exist, &
  is_contig => hdf_is_contig, is_chunked => hdf_is_chunked

!> below are procedure that need generic mapping (type or rank agnostic)

!> write group or dataset integer/real
generic, public :: write => hdf_write_scalar, hdf_write_1d, hdf_write_2d, hdf_write_3d, &
hdf_write_4d, hdf_write_5d, hdf_write_6d, hdf_write_7d

!> write attributes
generic, public :: writeattr => writeattr_char, writeattr_num

!> read attributes
generic, public :: readattr => readattr_char, readattr_num

!> read dataset
generic, public :: read => &
hdf_read_scalar, hdf_read_1d, hdf_read_2d, hdf_read_3d, hdf_read_4d,hdf_read_5d, hdf_read_6d, hdf_read_7d

!> private methods
!! each method must be declared here, and above as a generic, public
procedure,private :: &
hdf_write_scalar, hdf_write_1d, hdf_write_2d, hdf_write_3d, hdf_write_4d, hdf_write_5d, hdf_write_6d, hdf_write_7d, &
hdf_read_scalar, hdf_read_1d, hdf_read_2d, hdf_read_3d, hdf_read_4d, hdf_read_5d, hdf_read_6d, hdf_read_7d, &
writeattr_char, writeattr_num, readattr_char, readattr_num

end type hdf5_file

interface h5write
procedure lt0write, lt1write, lt2write, lt3write, lt4write, lt5write, lt6write, lt7write
end interface h5write

interface h5read
procedure lt0read, lt1read, lt2read, lt3read, lt4read, lt5read, lt6read, lt7read
end interface h5read


!> Submodules
interface

module logical function h5exist(filename, dname)
character(*), intent(in) :: filename, dname
end function h5exist


module subroutine lt0write(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value
integer, intent(out), optional :: ierr
end subroutine lt0write

module subroutine lt1write(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:)
integer, intent(out), optional :: ierr
end subroutine lt1write

module subroutine lt2write(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:)
integer, intent(out), optional :: ierr
end subroutine lt2write

module subroutine lt3write(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt3write

module subroutine lt4write(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt4write

module subroutine lt5write(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt5write

module subroutine lt6write(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt6write

module subroutine lt7write(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt7write

module subroutine lt0read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value
integer, intent(out), optional :: ierr
end subroutine lt0read

module subroutine lt1read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value(:)
integer, intent(out), optional :: ierr
end subroutine lt1read

module subroutine lt2read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value(:,:)
integer, intent(out), optional :: ierr
end subroutine lt2read

module subroutine lt3read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value(:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt3read

module subroutine lt4read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt4read

module subroutine lt5read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt5read

module subroutine lt6read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt6read

module subroutine lt7read(filename, dname, value, ierr)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
end subroutine lt7read


module subroutine hdf_write_scalar(self,dname,value, ierr)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value
integer, intent(out), optional :: ierr
end subroutine hdf_write_scalar

module subroutine hdf_write_1d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out), optional :: ierr
end subroutine hdf_write_1d

module subroutine hdf_write_2d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out), optional :: ierr
end subroutine hdf_write_2d

module subroutine hdf_write_3d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out), optional :: ierr
end subroutine hdf_write_3d

module subroutine hdf_write_4d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out), optional :: ierr
end subroutine hdf_write_4d

module subroutine hdf_write_5d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out), optional :: ierr
end subroutine hdf_write_5d

module subroutine hdf_write_6d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out), optional :: ierr
end subroutine hdf_write_6d

module subroutine hdf_write_7d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out), optional :: ierr
end subroutine hdf_write_7d


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

module logical function hdf_is_contig(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_is_contig

module logical function hdf_is_chunked(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_is_chunked


module subroutine hdf_read_scalar(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout)        :: value
!! intent(inout) for character
integer, intent(out), optional :: ierr
end subroutine hdf_read_scalar

module subroutine hdf_read_1d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_1d

module subroutine hdf_read_2d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_2d

module subroutine hdf_read_3d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_3d

module subroutine hdf_read_4d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_4d

module subroutine hdf_read_5d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_5d

module subroutine hdf_read_6d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:,:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_6d

module subroutine hdf_read_7d(self, dname, value, ierr, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:,:,:,:,:)
integer, intent(out), optional :: ierr
integer, intent(in), optional, dimension(:) :: istart, iend, stride
end subroutine hdf_read_7d


module subroutine hdf_open_group(self, gname, ierr)
class(hdf5_file), intent(inout) :: self
character(*), intent(in)        :: gname
integer, intent(out), optional :: ierr
end subroutine hdf_open_group

module subroutine hdf_close_group(self, ierr)
class(hdf5_file), intent(inout) :: self
integer, intent(out), optional :: ierr
end subroutine hdf_close_group

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
character(*), intent(in) :: dname, attr, attrval
integer, intent(out), optional :: ierr
end subroutine writeattr_char

module subroutine writeattr_num(self, dname, attr, attrval, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
class(*), intent(in) :: attrval(:)
integer, intent(out), optional :: ierr
end subroutine writeattr_num

end interface

contains


subroutine hdf_initialize(self,filename,ierr, status,action,comp_lvl,verbose,debug)
!! Opens hdf5 file

class(hdf5_file), intent(inout)    :: self
character(*), intent(in) :: filename
integer, intent(out), optional :: ierr
character(*), intent(in), optional :: status
character(*), intent(in), optional :: action
integer, intent(in), optional      :: comp_lvl
logical, intent(in), optional      :: verbose, debug

character(:), allocatable :: lstatus, laction
integer :: ier

if(self%is_open) then
  write(stderr,*) 'WARNING:h5fortran:initialize: file handle already open: '//self%filename
  return
endif

self%filename = filename

if (present(comp_lvl)) self%comp_lvl = comp_lvl
if (present(verbose)) self%verbose = verbose
if (present(debug)) self%debug = debug

!> Initialize FORTRAN interface.
call h5open_f(ier)
if(present(ierr)) ierr = ier
if (check(ier, 'ERROR: HDF5 library initialize')) then
  if (present(ierr)) return
  error stop
endif

!> get library version
call h5get_libversion_f(self%libversion(1), self%libversion(2), self%libversion(3), ier)
if (self%debug) print '(A,3I3)', 'HDF5 version: ',self%libversion
if(present(ierr)) ierr = ier
if (check(ier, 'ERROR: HDF5 library get version')) then
  if (present(ierr)) return
  error stop
endif

if(self%verbose) then
  call h5eset_auto_f(1, ier)
else
  call h5eset_auto_f(0, ier)
endif
if(present(ierr)) ierr = ier
if (check(ier, 'ERROR: HDF5 library set traceback')) then
  if (present(ierr)) return
  error stop
endif

lstatus = 'unknown'
if(present(status)) lstatus = toLower(status)

laction = 'rw'
if(present(action)) laction = toLower(action)

select case(lstatus)
case ('old', 'unknown')
  select case(laction)
    case('read','r')
      call h5fopen_f(filename, H5F_ACC_RDONLY_F, self%lid,ier)
    case('r+')
      call h5fopen_f(filename, H5F_ACC_RDWR_F, self%lid, ier)
    case('readwrite', 'rw', 'append', 'a')
      if(is_hdf5(filename)) then
        call h5fopen_f(filename, H5F_ACC_RDWR_F, self%lid, ier)
      else
        call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ier)
      endif
    case ('w','write')
      call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ier)
    case default
      write(stderr,*) 'Unsupported action -> ' // laction
      error stop 128
    end select
case('new','replace')
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ier)
case('scratch')
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ier)
  self%is_scratch = .true.
  if(.not.is_absolute_path(filename)) self%filename = get_tempdir() // '/' // filename
case default
  write(stderr,*) 'Unsupported status -> '// lstatus
  error stop 128
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
  write(stderr,*) 'WARNING:h5fortran:finalize: file handle is already closed: '// self%filename
  return
endif

!> close hdf5 file
call h5fclose_f(self%lid, ier)
if (present(ierr)) ierr = ier
if (check(ier, 'ERROR:finalize: HDF5 file close: ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

if (present(close_hdf5_interface)) then
  if (close_hdf5_interface) then
    call h5close_f(ier)
    if (present(ierr)) ierr = ier
    if (check(ier, 'ERROR: HDF5 library close')) then
      if (present(ierr)) return
      error stop
    endif
  endif
endif
!> sentinel lid
self%lid = 0

if(self%is_scratch) then
  if (unlink(self%filename)) write(stderr,*) 'WARNING: could not delete scratch file: ' // self%filename
endif

self%is_open = .false.

end subroutine hdf_finalize


subroutine hdf_flush(self, ierr)

class(hdf5_file), intent(in) :: self
integer, intent(out), optional :: ierr
integer :: ier

call h5fflush_f(self%lid, H5F_SCOPE_GLOBAL_F, ier)
if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: HDF5 flush ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

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
if (check(ier, 'ERROR: HDF5 library close')) then
  if (present(ierr)) return
  error stop
endif

end subroutine hdf5_close


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

check = .true.
fn = ""
dn = ""
if (present(filename)) fn = filename
if (present(dname)) dn = dname

select case (ierr)
case (0)
  check = .false.
  return
case (6)
  write(stderr,*) 'ERROR: ' // fn // ':' // dname // ' datatype is not handled by h5fortran.'
case (128)
  write(stderr,*) 'ERROR:initialize ' // fn // ' could not be opened or created'
case default
  write(stderr,*) 'ERROR: ' // fn // ':' // dn // ' error code ', ierr
end select

end function check


subroutine hdf_wrapup(did, sid, ierr)
integer(HID_T), intent(in) :: sid, did
integer, intent(out) :: ierr

if(sid /= 0) then
  call h5sclose_f(sid, ierr)
  if (check(ierr, 'ERROR:h5sclose dataspace')) return
endif

call h5dclose_f(did, ierr)
if (check(ierr, 'ERROR:h5dclose dataset')) return

end subroutine hdf_wrapup


subroutine hdf_get_slice(self, dname, did, sid, mem_sid, ierr, i0, i1, i2)
!! setup array slices for read and write
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(hid_t), intent(out) :: sid, did, mem_sid
integer, intent(out) :: ierr
class(*), intent(in), dimension(:) :: i0, i1
class(*), intent(in), optional, dimension(:) :: i2

integer(hsize_t), dimension(size(i0)) :: istart, iend, stride, mem_dims
integer :: mem_rank

if(.not.self%is_open) error stop 'h5fortran:slice: file handle is not open'

!! istart
select type (i0)
type is (integer(int32))
  istart = int(i0, int64)
type is (integer(hsize_t))
  istart = i0
class default
  ierr = -1
  write(stderr,*) 'ERROR: wrong integer type for istart: ', dname, self%filename
  return
end select

!! iend

select type (i1)
type is (integer(int32))
  iend = int(i1, int64)
type is (integer(hsize_t))
  iend = i1
class default
  ierr = -1
  write(stderr,*) 'ERROR: wrong integer type for iend: ', dname, self%filename
  return
end select

!! stride

if (present(i2)) then
  select type (i2)
  type is (integer(int32))
    stride = int(i2, int64)
  type is (integer(hsize_t))
    stride = i2
  class default
    ierr = -1
    write(stderr,*) 'ERROR: wrong integer type for stride: ', dname, self%filename
    return
  end select
  if(self%debug) print *,'DEBUG: user-stride:',stride
else
  stride = 1
  if(self%debug) print *, 'DEBUG: auto-stride',stride
endif

!! compensate for 0-based hyperslab vs. 1-based Fortran
istart = istart - 1

mem_dims = iend - istart
mem_rank = size(mem_dims)


call h5dopen_f(self%lid, dname, did, ierr)

if(ierr == 0) call h5dget_space_f(did, sid, ierr)

if(ierr == 0) call h5sselect_hyperslab_f(sid, H5S_SELECT_SET_F, istart, mem_dims, ierr, stride=stride)

if(ierr == 0) call h5screate_simple_f(mem_rank, mem_dims, mem_sid, ierr)

if (ierr /= 0) then
  write(stderr,*) 'ERROR:get_slice:', dname, self%filename
  return
endif


end subroutine hdf_get_slice


subroutine hdf_shape_check(self, dname, dims, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(in) :: dims(:)
integer, intent(out) :: ierr

integer(SIZE_T) :: dsize
integer(HSIZE_T) :: ddims(size(dims))
integer :: dtype, drank

if(.not.self%is_open) error stop 'h5fortran:shape: file handle is not open'

if (.not.self%exist(dname)) then
  write(stderr,*) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_dataset_ndims_f(self%lid, dname, drank, ierr)
if (check(ierr, 'ERROR: get_dataset_ndim ' // dname // ' read ' // self%filename)) return

if (drank /= size(dims)) then
  write(stderr,'(A,I6,A,I6)') 'ERROR: rank mismatch ' // dname // ' = ',drank,'  variable rank =', size(dims)
  ierr = -1
  return
endif

!> check for matching size, else bad reads can occur.

call h5ltget_dataset_info_f(self%lid, dname, ddims, dtype, dsize, ierr)
if (check(ierr, 'ERROR: get_dataset_info ' // dname // ' read ' // self%filename)) return

if(.not. all(dims == ddims)) then
  write(stderr,*) 'ERROR: shape mismatch ' // dname // ' = ',ddims,'  variable shape =', dims
  ierr = -1
  return
endif

end subroutine hdf_shape_check


end module h5fortran
