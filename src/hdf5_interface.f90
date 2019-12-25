module hdf5_interface
!! HDF5 object-oriented polymorphic interface
use, intrinsic:: iso_fortran_env, only: real32, real64, int32, stderr=>error_unit
use H5LT, only: HID_T, SIZE_T, HSIZE_T, H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
    h5open_f, h5close_f, h5gcreate_f, h5gclose_f, h5fopen_f, h5fcreate_f, h5fclose_f, h5lexists_f

use string_utils, only : toLower, strip_trailing_null, truncate_string_null

implicit none

!> main type
type :: hdf5_file

character(:),allocatable  :: filename
integer(HID_T) :: lid, &   !< location identifier
                  gid, &    !< group identifier
                  glid, &   !< group location identifier
                  sid, did, pid

integer  :: comp_lvl = 0 !< compression level (1-9)  0: disable compression
integer(HSIZE_T) :: chunk_size(7) = [64,64,1,1,1,1,1]  !< chunk size per dimension (arbitrary)
logical :: verbose=.false.

contains
!> initialize HDF5 file
procedure, public :: initialize => hdf_initialize, finalize => hdf_finalize, writeattr, &
  open => hdf_open_group, close => hdf_close_group, shape => hdf_get_shape

!> add group or dataset integer/real
generic, public   :: add => &
hdf_add_group, hdf_add_int, hdf_add_int_1d, hdf_add_int_2d, hdf_add_int_3d, hdf_add_int_5d, hdf_add_int_6d, hdf_add_int_7d,&
hdf_add_real32, hdf_add_real32_1d, hdf_add_real32_2d, hdf_add_real32_3d, &
  hdf_add_real32_4d, hdf_add_real32_5d, hdf_add_real32_6d, hdf_add_real32_7d, &
hdf_add_real64, hdf_add_real64_1d, hdf_add_real64_2d, hdf_add_real64_3d, &
  hdf_add_real64_4d, hdf_add_real64_5d, hdf_add_real64_6d, hdf_add_real64_7d, &
hdf_add_string

!> get dataset integer/real
generic, public   :: get => &
hdf_get_int, hdf_get_int_1d, hdf_get_int_2d, hdf_get_int_3d, hdf_get_int_4d,hdf_get_int_5d, hdf_get_int_6d, hdf_get_int_7d,&
hdf_get_real32, hdf_get_real32_1d, hdf_get_real32_2d, hdf_get_real32_3d, &
  hdf_get_real32_4d, hdf_get_real32_5d, hdf_get_real32_6d, hdf_get_real32_7d, &
hdf_get_real64, hdf_get_real64_1d, hdf_get_real64_2d, hdf_get_real64_3d, &
  hdf_get_real64_4d, hdf_get_real64_5d, hdf_get_real64_6d, hdf_get_real64_7d,  &
hdf_get_string


!> private methods
procedure,private :: hdf_add_group, &
hdf_add_int, hdf_add_int_1d, hdf_add_int_2d, hdf_add_int_3d, hdf_add_int_4d, hdf_add_int_5d, hdf_add_int_6d, hdf_add_int_7d, &
hdf_get_int, hdf_get_int_1d, hdf_get_int_2d, hdf_get_int_3d, hdf_get_int_4d, hdf_get_int_5d, hdf_get_int_6d, hdf_get_int_7d, &
hdf_add_real32, hdf_add_real32_1d, hdf_add_real32_2d, hdf_add_real32_3d, &
  hdf_add_real32_4d, hdf_add_real32_5d, hdf_add_real32_6d, hdf_add_real32_7d, &
hdf_add_real64, hdf_add_real64_1d, hdf_add_real64_2d, hdf_add_real64_3d, &
  hdf_add_real64_4d, hdf_add_real64_5d, hdf_add_real64_6d, hdf_add_real64_7d, &
hdf_get_real32, hdf_get_real32_1d, hdf_get_real32_2d, hdf_get_real32_3d, &
  hdf_get_real32_4d, hdf_get_real32_5d, hdf_get_real32_6d, hdf_get_real32_7d, &
hdf_get_real64, hdf_get_real64_1d, hdf_get_real64_2d, hdf_get_real64_3d, &
  hdf_get_real64_4d, hdf_get_real64_5d, hdf_get_real64_6d, hdf_get_real64_7d, &
hdf_add_string, hdf_get_string

end type hdf5_file


!> Submodules
interface

module subroutine hdf_setup_write(self, dname, dtype, dims, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), intent(in) :: dims(:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_setup_write

module subroutine hdf_wrapup(self, ierr)
class(hdf5_file), intent(in) :: self
integer, intent(out) :: ierr
end subroutine hdf_wrapup

module subroutine hdf_set_deflate(self, dims, ierr)
class(hdf5_file), intent(inout) :: self
integer(HSIZE_T), intent(in) :: dims(:)
integer, intent(out) :: ierr
end subroutine hdf_set_deflate

module subroutine hdf_add_string(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname, value
integer, intent(out) :: ierr
end subroutine hdf_add_string

module subroutine hdf_add_int(self, dname, value, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value
integer, intent(out) :: ierr
end subroutine hdf_add_int

module subroutine hdf_add_int_1d(self,dname,value, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:)
integer, intent(out) :: ierr
end subroutine hdf_add_int_1d

module subroutine hdf_add_int_2d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_int_2d

module subroutine hdf_add_int_3d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_int_3d

module subroutine hdf_add_int_4d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer, intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_int_4d

module subroutine hdf_add_int_5d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_int_5d

module subroutine hdf_add_int_6d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_int_6d

module subroutine hdf_add_int_7d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(int32), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_int_7d

module subroutine hdf_add_real64(self,dname,value, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value
integer, intent(out) :: ierr
end subroutine hdf_add_real64

module subroutine hdf_add_real64_1d(self,dname,value, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real64_1d

module subroutine hdf_add_real64_2d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real64_2d

module subroutine hdf_add_real64_3d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real64_3d

module subroutine hdf_add_real64_4d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real64_4d

module subroutine hdf_add_real64_5d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real64_5d

module subroutine hdf_add_real64_6d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real64_6d

module subroutine hdf_add_real64_7d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real64_7d

module subroutine hdf_add_real32(self,dname,value, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value
integer, intent(out) :: ierr
end subroutine hdf_add_real32

module subroutine hdf_add_real32_1d(self,dname,value, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real32_1d

module subroutine hdf_add_real32_2d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real32_2d

module subroutine hdf_add_real32_3d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real32_3d

module subroutine hdf_add_real32_4d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real32_4d

module subroutine hdf_add_real32_5d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real32_5d

module subroutine hdf_add_real32_6d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real32_6d

module subroutine hdf_add_real32_7d(self,dname,value, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
integer, intent(out) :: ierr
end subroutine hdf_add_real32_7d

module subroutine hdf_get_shape(self, dname, dims, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(HSIZE_T), intent(out), allocatable :: dims(:)
integer, intent(out) :: ierr
end subroutine hdf_get_shape

module subroutine hdf_get_string(self,dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
character(*), intent(out) :: value
integer, intent(out) :: ierr
end subroutine hdf_get_string

module subroutine hdf_get_int(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out)             :: value
integer, intent(out) :: ierr
end subroutine hdf_get_int

module subroutine hdf_get_int_1d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out),allocatable :: value(:)
integer, intent(out) :: ierr
end subroutine hdf_get_int_1d

module subroutine hdf_get_int_2d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out),allocatable :: value(:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_int_2d

module subroutine hdf_get_int_3d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out),allocatable :: value(:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_int_3d

module subroutine hdf_get_int_4d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out),allocatable :: value(:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_int_4d

module subroutine hdf_get_int_5d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out),allocatable :: value(:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_int_5d

module subroutine hdf_get_int_6d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out),allocatable :: value(:,:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_int_6d

module subroutine hdf_get_int_7d(self, dname, value, ierr)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer(int32), intent(out),allocatable :: value(:,:,:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_int_7d


module subroutine hdf_get_real32(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out) :: value
integer, intent(out) :: ierr
end subroutine hdf_get_real32

module subroutine hdf_get_real32_1d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:)
integer, intent(out) :: ierr
end subroutine hdf_get_real32_1d

module subroutine hdf_get_real32_2d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real32_2d

module subroutine hdf_get_real32_3d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real32_3d

module subroutine hdf_get_real32_4d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real32_4d

module subroutine hdf_get_real32_5d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real32_5d

module subroutine hdf_get_real32_6d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real32_6d

module subroutine hdf_get_real32_7d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real32_7d

module subroutine hdf_get_real64(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out) :: value
integer, intent(out) :: ierr
end subroutine hdf_get_real64

module subroutine hdf_get_real64_1d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:)
integer, intent(out) :: ierr
end subroutine hdf_get_real64_1d

module subroutine hdf_get_real64_2d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real64_2d

module subroutine hdf_get_real64_3d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real64_3d

module subroutine hdf_get_real64_4d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real64_4d

module subroutine hdf_get_real64_5d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real64_5d

module subroutine hdf_get_real64_6d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real64_6d

module subroutine hdf_get_real64_7d(self, dname, value, ierr)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:,:,:,:,:)
integer, intent(out) :: ierr
end subroutine hdf_get_real64_7d

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

integer, parameter :: ENOENT = 2, EIO = 5

private
public :: hdf5_file, toLower, hsize_t, strip_trailing_null, truncate_string_null



contains


subroutine hdf_initialize(self,filename,ierr, status,action,comp_lvl)
!! Opens hdf5 file

class(hdf5_file), intent(inout)    :: self
character(*), intent(in)           :: filename
integer, intent(out)               :: ierr
character(*), intent(in), optional :: status
character(*), intent(in), optional :: action
integer, intent(in), optional      :: comp_lvl

character(:), allocatable :: lstatus, laction
logical :: exists

self%filename = filename

if (present(comp_lvl)) self%comp_lvl = comp_lvl

!> Initialize FORTRAN interface.
call h5open_f(ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: HDF5 library initialize'
  return
endif

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
          ierr = ENOENT
          return
        endif
        call h5fopen_f(filename,H5F_ACC_RDONLY_F,self%lid,ierr)
      case('write','readwrite','w','rw', 'r+', 'append', 'a')
        call h5fopen_f(filename,H5F_ACC_RDWR_F,self%lid,ierr)
        if (ierr /= 0) then
          write(stderr,*) 'ERROR: ' // filename // ' could not be opened'
          ierr = EIO
          return
        endif
      case default
        write(stderr,*) 'Unsupported action -> ' // laction
        ierr = 128
        return
      endselect
  case('new','replace')
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%lid, ierr)
    if (ierr /= 0) then
      write(stderr,*) 'ERROR: ' // filename // ' could not be created'
      ierr = EIO
      return
    endif
  case default
    write(stderr,*) 'Unsupported status ->'// lstatus
    ierr = 128
    return
endselect

end subroutine hdf_initialize


subroutine hdf_finalize(self, ierr)
class(hdf5_file), intent(in) :: self
integer, intent(out) :: ierr

!> close hdf5 file
call h5fclose_f(self%lid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: HDF5 file close: ' // self%filename
  return
endif

!>  Close Fortran interface.
call h5close_f(ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: HDF5 library close'
  return
endif

end subroutine hdf_finalize


subroutine hdf_add_group(self, gname, ierr)
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
  if (ierr /= 0) then
    write(stderr,*) 'ERROR: did not find group ' // gname // ' in ' // self%filename
    return
  endif

  if(.not.gexist) then
    call h5gcreate_f(self%lid, gname(1:sp-1), gid, ierr)
    if (ierr /= 0) then
      write(stderr,*) 'ERROR: creating group ' // gname // ' in ' // self%filename
      return
    endif

    call h5gclose_f(gid, ierr)
    if (ierr /= 0) then
      write(stderr,*) 'ERROR: closing group ' // gname // ' in ' // self%filename
      return
    endif
  endif
end do

end subroutine hdf_add_group

end module hdf5_interface
