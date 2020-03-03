module h5fortran
!! HDF5 object-oriented polymorphic interface
use, intrinsic :: iso_c_binding, only : c_ptr, c_loc
use, intrinsic :: iso_fortran_env, only : real32, real64, int64, int32, stderr=>error_unit
use hdf5, only : HID_T, SIZE_T, HSIZE_T, H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
    h5open_f, h5close_f, h5gcreate_f, h5gclose_f, h5fopen_f, h5fcreate_f, h5fclose_f, h5lexists_f, &
    h5get_libversion_f, h5eset_auto_f
use h5lt, only : h5ltget_dataset_ndims_f, h5ltget_dataset_info_f

use string_utils, only : toLower, strip_trailing_null, truncate_string_null

implicit none
private
public :: hdf5_file, toLower, hdf_shape_check, hsize_t, strip_trailing_null, truncate_string_null, &
  check, h5write, h5read

!> Workaround for Intel 19.1 / 2020 bug with /stand:f18
!> error #6410: This name has not been declared as an array or a function.   [RANK]
intrinsic :: rank

!> main type
type :: hdf5_file

character(:),allocatable  :: filename
integer(HID_T) :: lid=0, &   !< location ID
                  gid, &    !< group ID
                  glid   !< group location ID

integer :: comp_lvl = 0 !< compression level (1-9)  0: disable compression
integer(HSIZE_T) :: chunk_size(7) = [1,1,1,1,1,1,1]  !< chunk size per dimension
logical :: verbose=.true.
integer :: libversion(3)  !< major, minor, rel

contains
!> initialize HDF5 file
procedure, public :: initialize => hdf_initialize, finalize => hdf_finalize, writeattr, &
  open => hdf_open_group, close => hdf_close_group, shape => hdf_get_shape, layout => hdf_get_layout, &
  exist => hdf_check_exist, exists => hdf_check_exist, is_contig => hdf_is_contig, is_chunked => hdf_is_chunked

!> write group or dataset integer/real
generic, public   :: write => hdf_write_scalar, hdf_write_1d, hdf_write_2d, hdf_write_3d, &
hdf_write_4d, hdf_write_5d, hdf_write_6d, hdf_write_7d, hdf_write_group

!> read dataset integer/real
generic, public   :: read => &
hdf_read_scalar, hdf_read_1d, hdf_read_2d, hdf_read_3d, &
  hdf_read_4d,hdf_read_5d, hdf_read_6d, hdf_read_7d

!> private methods
procedure,private :: hdf_write_group, &
hdf_write_scalar, hdf_write_1d, hdf_write_2d, hdf_write_3d, &
  hdf_write_4d, hdf_write_5d, hdf_write_6d, hdf_write_7d, &
hdf_read_scalar, hdf_read_1d, hdf_read_2d, hdf_read_3d, &
  hdf_read_4d, hdf_read_5d, hdf_read_6d, hdf_read_7d

end type hdf5_file

interface h5write
  procedure lt0write, lt1write, lt2write, lt3write, lt4write, lt5write, lt6write, lt7write
end interface h5write

interface h5read
  procedure lt0read, lt1read, lt2read, lt3read, lt4read, lt5read, lt6read, lt7read
end interface h5read


!> Submodules
interface

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


module subroutine hdf_setup_write(self, dname, dtype, dims, sid, did, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out) :: sid, did
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_setup_write

module subroutine hdf_write_scalar(self,dname,value, ierr)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value
integer, intent(out) :: ierr
end subroutine hdf_write_scalar

module subroutine hdf_write_1d(self,dname,value, ierr)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:)
integer, intent(out) :: ierr
end subroutine hdf_write_1d

module subroutine hdf_write_2d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out) :: ierr
end subroutine hdf_write_2d

module subroutine hdf_write_3d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out) :: ierr
end subroutine hdf_write_3d

module subroutine hdf_write_4d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out) :: ierr
end subroutine hdf_write_4d

module subroutine hdf_write_5d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out) :: ierr
end subroutine hdf_write_5d

module subroutine hdf_write_6d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out) :: ierr
end subroutine hdf_write_6d

module subroutine hdf_write_7d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(rank(value))
integer, intent(out) :: ierr
end subroutine hdf_write_7d


module subroutine hdf_get_shape(self, dname, dims, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(HSIZE_T), intent(out), allocatable :: dims(:)
integer, intent(out) :: ierr
end subroutine hdf_get_shape

module integer function hdf_get_layout(self, dname) result(layout)
!! H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_VIRTUAL_F, H5D_COMPACT_F
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
end function hdf_get_layout

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
class(*), intent(inout)      :: value
integer, intent(out) :: ierr
end subroutine hdf_read_scalar

module subroutine hdf_read_1d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:)
integer, intent(out) :: ierr
end subroutine hdf_read_1d

module subroutine hdf_read_2d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:)
integer, intent(out) :: ierr
end subroutine hdf_read_2d

module subroutine hdf_read_3d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_read_3d

module subroutine hdf_read_4d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_read_4d

module subroutine hdf_read_5d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_read_5d

module subroutine hdf_read_6d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_read_6d

module subroutine hdf_read_7d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:,:,:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_read_7d


module subroutine hdf_open_group(self, gname, ierr)
class(hdf5_file), intent(inout) :: self
character(*), intent(in)        :: gname
integer, intent(out) :: ierr
end subroutine hdf_open_group

module subroutine hdf_close_group(self, ierr)
class(hdf5_file), intent(inout) :: self
integer, intent(out) :: ierr
end subroutine hdf_close_group

module subroutine writeattr(self,dname,attr,attrval, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr, attrval
integer, intent(out) :: ierr
end subroutine writeattr

end interface

contains


subroutine hdf_initialize(self,filename,ierr, status,action,comp_lvl,chunk_size,verbose)
!! Opens hdf5 file

class(hdf5_file), intent(inout)    :: self
character(*), intent(in)           :: filename
integer, intent(out)               :: ierr
character(*), intent(in), optional :: status
character(*), intent(in), optional :: action
integer, intent(in), optional      :: comp_lvl
class(*), intent(in), optional     :: chunk_size(7)
logical, intent(in), optional      :: verbose

character(:), allocatable :: lstatus, laction
logical :: exists

self%filename = filename

if (present(comp_lvl)) self%comp_lvl = comp_lvl
if (present(verbose)) self%verbose = verbose

if (present(chunk_size)) then
  select type(chunk_size)
  type is (integer(int32))
    self%chunk_size = chunk_size
  type is (integer(int64))
    self%chunk_size = chunk_size
  class default
    write(stderr,*) 'ERROR: chunk_size is rank-1, size-7 vector of {int32,int64}'
  end select
endif

!> Initialize FORTRAN interface.
call h5open_f(ierr)
if (check(ierr, 'ERROR: HDF5 library initialize')) return

!> get library version
call h5get_libversion_f(self%libversion(1), self%libversion(2), self%libversion(3), ierr)
! if (self%verbose) print '(A,3I3)', 'HDF5 version: ',self%libversion
if (check(ierr, 'ERROR: HDF5 library get version')) return

if(self%verbose) then
  call h5eset_auto_f(1, ierr)
else
  call h5eset_auto_f(0, ierr)
endif
if (check(ierr, 'ERROR: HDF5 library set traceback')) return

lstatus = 'old'
if(present(status)) lstatus = toLower(status)

laction = 'rw'
if(present(action)) laction = toLower(action)

select case(lstatus)
case ('old', 'unknown')
  select case(laction)
    case('read','r')  !< Open an existing file.
      inquire(file=filename, exist=exists)
      if (.not.exists) then
        write(stderr,*) 'ERROR: ' // filename // ' does not exist.'
        ierr = -1
        return
      endif
      call h5fopen_f(filename,H5F_ACC_RDONLY_F,self%lid,ierr)
    case('write','readwrite','w','rw', 'r+', 'append', 'a')
      inquire(file=filename, exist=exists)
      if(lstatus == 'unknown' .and. .not.exists) then
        call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ierr)
        if (check(ierr, 'ERROR: ' // filename // ' could not be created')) return
      else
        call h5fopen_f(filename, H5F_ACC_RDWR_F, self%lid, ierr)
        if (check(ierr, 'ERROR: ' // filename // ' could not be opened in read/write')) return
      endif
    case default
      write(stderr,*) 'Unsupported action -> ' // laction
      ierr = 128
    endselect
case('new','replace')
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ierr)
  if (check(ierr, 'ERROR: ' // filename // ' could not be created')) return
case default
  write(stderr,*) 'Unsupported status -> '// lstatus
  ierr = 128
endselect

end subroutine hdf_initialize


subroutine hdf_finalize(self, ierr)
class(hdf5_file), intent(inout) :: self
integer, intent(out) :: ierr

!> close hdf5 file
call h5fclose_f(self%lid, ierr)
if (check(ierr, 'ERROR: HDF5 file close: ' // self%filename)) return

!>  Close Fortran interface.
call h5close_f(ierr)
if (check(ierr, 'ERROR: HDF5 library close')) return

!> sentinel lid
self%lid = 0

end subroutine hdf_finalize


subroutine hdf_write_group(self, gname, ierr)
!! NOTE: needs to stay here instead of in submodule apparently?

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: gname    !< relative path to group
integer, intent(out) :: ierr

integer(HID_T)  :: gid

integer :: sp, ep, sl
logical :: gexist

sl = len(gname)
sp = 1
ep = 0

do
  ep = index(gname(sp+1:sl), "/")

  ! no subgroup found
  if (ep == 0) exit

  ! check subgroup exists
  sp = sp + ep
  call h5lexists_f(self%lid, gname(1:sp-1), gexist, ierr)
  if (check(ierr, 'ERROR: did not find group ' // gname // ' in ' // self%filename)) return

  if(.not.gexist) then
    call h5gcreate_f(self%lid, gname(1:sp-1), gid, ierr)
    if (check(ierr, 'ERROR: creating group ' // gname // ' in ' // self%filename)) return

    call h5gclose_f(gid, ierr)
    if (check(ierr, 'ERROR: closing group ' // gname // ' in ' // self%filename)) return
  endif
end do

end subroutine hdf_write_group


logical function check(ierr, msg)
integer, intent(in) :: ierr
character(*), intent(in) :: msg

check = ierr /= 0
if (.not.check) return

write(stderr, *) msg

end function check


subroutine hdf_shape_check(self, dname, dims, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(in) :: dims(:)
integer, intent(out) :: ierr

integer(SIZE_T) :: dsize
integer(HSIZE_T) :: ddims(size(dims))
integer :: dtype, drank

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
