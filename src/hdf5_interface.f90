module hdf5_interface
!! HDF5 object-oriented polymorphic interface

use, intrinsic:: iso_fortran_env, only: real32, real64, stderr=>error_unit
use H5LT

implicit none

!> main type
type :: hdf5_file

character(:),allocatable  :: filename
integer(HID_T) :: lid, &   !< location identifier
                  gid, &    !< group identifier
                  glid, &   !< group location identifier
                  sid, did, pid

integer :: comp_lvl = 0 !< compression level (1-9)  0: disable compression
integer(HSIZE_T) :: chunk_size(6) = [64,64,1,1,1,1]  !< chunk size per dimension (arbitrary)
logical :: verbose=.false.


contains
!> initialize HDF5 file
procedure, public :: initialize => hdf_initialize, finalize => hdf_finalize, writeattr, &
  open => hdf_open_group, close => hdf_close_group

!> add group or dataset integer/real 
generic, public   :: add => &
hdf_add_group, hdf_add_int, hdf_add_int_1d, hdf_add_int_2d, hdf_add_int_3d, &
hdf_add_real32, hdf_add_real32_1d, hdf_add_real32_2d, hdf_add_real32_3d, hdf_add_real32_4d, hdf_add_real32_5d, hdf_add_real32_6d, &
hdf_add_real64, hdf_add_real64_1d, hdf_add_real64_2d, hdf_add_real64_3d, hdf_add_real64_4d, hdf_add_real64_5d, hdf_add_real64_6d, &
hdf_add_string

!> get dataset integer/real
generic, public   :: get => &
hdf_get_int, hdf_get_int_1d, hdf_get_int_2d, hdf_get_int_3d,&
hdf_get_real32, hdf_get_real32_1d, hdf_get_real32_2d, hdf_get_real32_3d, hdf_get_real32_4d, hdf_get_real32_5d, hdf_get_real32_6d, &
hdf_get_real64, hdf_get_real64_1d, hdf_get_real64_2d, hdf_get_real64_3d, hdf_get_real64_4d, hdf_get_real64_5d, hdf_get_real64_6d, &
hdf_get_string


!> private methods
procedure,private :: hdf_add_group, &
hdf_add_int, hdf_add_int_1d, hdf_add_int_2d, hdf_add_int_3d, &
hdf_get_int, hdf_get_int_1d, hdf_get_int_2d, hdf_get_int_3d, &
hdf_add_real32, hdf_add_real32_1d, hdf_add_real32_2d, hdf_add_real32_3d, hdf_add_real32_4d, hdf_add_real32_5d,  hdf_add_real32_6d, &
hdf_add_real64, hdf_add_real64_1d, hdf_add_real64_2d, hdf_add_real64_3d, hdf_add_real64_4d, hdf_add_real64_5d, hdf_add_real64_6d, &
hdf_get_real32, hdf_get_real32_1d, hdf_get_real32_2d, hdf_get_real32_3d, hdf_get_real32_4d, hdf_get_real32_5d, hdf_get_real32_6d, &
hdf_get_real64, hdf_get_real64_1d, hdf_get_real64_2d, hdf_get_real64_3d, hdf_get_real64_4d, hdf_get_real64_5d, hdf_get_real64_6d, &
hdf_add_string, hdf_get_string
  
end type hdf5_file


!> Submodules
interface

module subroutine hdf_setup_write(self, dname, dtype, dims, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), intent(in) :: dims(:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_setup_write

module subroutine hdf_wrapup(self)
class(hdf5_file), intent(in) :: self
end subroutine hdf_wrapup

module subroutine hdf_set_deflate(self, dims)
class(hdf5_file), intent(inout) :: self
integer(HSIZE_T), intent(in) :: dims(:)
end subroutine hdf_set_deflate

module subroutine hdf_add_string(self,dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname, value
end subroutine hdf_add_string

module subroutine hdf_add_int(self,dname,value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(in) :: value
end subroutine hdf_add_int

module subroutine hdf_add_int_1d(self,dname,value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(in) :: value(:)
end subroutine hdf_add_int_1d

module subroutine hdf_add_int_2d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer, intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_int_2d

module subroutine hdf_add_int_3d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer, intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_int_3d

module subroutine hdf_add_real64(self,dname,value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value
end subroutine hdf_add_real64

module subroutine hdf_add_real64_1d(self,dname,value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:)
end subroutine hdf_add_real64_1d

module subroutine hdf_add_real64_2d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real64_2d

module subroutine hdf_add_real64_3d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real64_3d

module subroutine hdf_add_real64_4d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real64_4d

module subroutine hdf_add_real64_5d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real64_5d

module subroutine hdf_add_real64_6d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real64_6d

module subroutine hdf_add_real32(self,dname,value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value
end subroutine hdf_add_real32

module subroutine hdf_add_real32_1d(self,dname,value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:)
end subroutine hdf_add_real32_1d

module subroutine hdf_add_real32_2d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real32_2d

module subroutine hdf_add_real32_3d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real32_3d

module subroutine hdf_add_real32_4d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real32_4d

module subroutine hdf_add_real32_5d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real32_5d

module subroutine hdf_add_real32_6d(self,dname,value, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: value(:,:,:,:,:,:)
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_add_real32_6d

module subroutine hdf_get_string(self,dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
character(*), intent(out) :: value
end subroutine hdf_get_string

module subroutine hdf_get_int(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer, intent(out)             :: value
end subroutine hdf_get_int

module subroutine hdf_get_int_1d(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer, intent(out),allocatable :: value(:)
end subroutine hdf_get_int_1d

module subroutine hdf_get_int_2d(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer, intent(out),allocatable :: value(:,:)
end subroutine hdf_get_int_2d

module subroutine hdf_get_int_3d(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
integer, intent(out),allocatable :: value(:,:,:)
end subroutine hdf_get_int_3d

module subroutine hdf_get_real32(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out) :: value
end subroutine hdf_get_real32

module subroutine hdf_get_real32_1d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:)
end subroutine hdf_get_real32_1d

module subroutine hdf_get_real32_2d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:)
end subroutine hdf_get_real32_2d

module subroutine hdf_get_real32_3d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:)
end subroutine hdf_get_real32_3d

module subroutine hdf_get_real32_4d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:,:)
end subroutine hdf_get_real32_4d

module subroutine hdf_get_real32_5d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:,:,:)
end subroutine hdf_get_real32_5d

module subroutine hdf_get_real32_6d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real32), intent(out),allocatable :: value(:,:,:,:,:,:)
end subroutine hdf_get_real32_6d

module subroutine hdf_get_real64(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out) :: value
end subroutine hdf_get_real64

module subroutine hdf_get_real64_1d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:)
end subroutine hdf_get_real64_1d

module subroutine hdf_get_real64_2d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:)
end subroutine hdf_get_real64_2d

module subroutine hdf_get_real64_3d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:)
end subroutine hdf_get_real64_3d

module subroutine hdf_get_real64_4d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:,:)
end subroutine hdf_get_real64_4d

module subroutine hdf_get_real64_5d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:,:,:)
end subroutine hdf_get_real64_5d

module subroutine hdf_get_real64_6d(self, dname, value)
class(hdf5_file), intent(in)  :: self
character(*), intent(in)      :: dname
real(real64), intent(out),allocatable :: value(:,:,:,:,:,:)
end subroutine hdf_get_real64_6d

module subroutine hdf_open_group(self, gname)
class(hdf5_file), intent(inout) :: self
character(*), intent(in)        :: gname
end subroutine hdf_open_group

module subroutine hdf_close_group(self)
class(hdf5_file), intent(inout) :: self
end subroutine hdf_close_group

module subroutine writeattr(self,dname,attr,attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr, attrval
end subroutine writeattr

end interface


public :: hdf5_file, toLower

private

contains
!=============================================================================
subroutine hdf_initialize(self,filename,status,action,comp_lvl)
!< Opens hdf5 file

class(hdf5_file), intent(inout)    :: self
character(*), intent(in)           :: filename
character(*), intent(in), optional :: status
character(*), intent(in), optional :: action
integer, intent(in), optional      :: comp_lvl

character(:), allocatable :: lstatus, laction
integer :: ierr

self%filename = filename

if (present(comp_lvl)) self%comp_lvl = comp_lvl

!> Initialize FORTRAN interface.
call h5open_f(ierr)
if (ierr /= 0) error stop 'Error: HDF5 library initialize Failed!'

lstatus = 'old'  ! not merge() due to unequal character length
if(present(status)) lstatus = toLower(status)

laction = 'rw'  ! not merge() due to unequal character length
if(present(action)) laction = toLower(action)


select case(lstatus)
  case ('old')
    select case(laction)
      case('read','r')  !> Open an existing file.
        call h5fopen_f(filename,H5F_ACC_RDONLY_F,self%lid,ierr)
      case('write','readwrite','w','rw')
        call h5fopen_f(filename,H5F_ACC_RDWR_F,self%lid,ierr)
      case default
        error stop 'Error: Unsupported action ->'// laction
      endselect
  case('new','replace')
    call h5fcreate_f(filename,H5F_ACC_TRUNC_F,self%lid,ierr)
  case default
    error stop 'Error: Unsupported status ->'// lstatus
endselect

if (ierr /= 0) error stop 'Error: HDF5 open/create failed: '//filename

end subroutine hdf_initialize


subroutine hdf_finalize(self)
class(hdf5_file), intent(in) :: self

integer :: ierr

!> close hdf5 file
call h5fclose_f(self%lid, ierr)

!>  Close Fortran interface.
call h5close_f(ierr)

if (ierr /= 0) error stop 'Error: HDF5 finalization: '//self%filename

end subroutine hdf_finalize


subroutine hdf_add_group(self, gname)
!! NOTE: needs to stay here instead of in submodule apparently?

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: gname    !< relative path to group

integer(HID_T)  :: gid

integer :: ierr, sp, ep, sl
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
  if (ierr /= 0) error stop 'problem finding group '//gname

  if(.not.gexist) then
    call h5gcreate_f(self%lid, gname(1:sp-1), gid, ierr)
    if (ierr /= 0) error stop 'problem creating group '//gname
   
    call h5gclose_f(gid, ierr)
    if (ierr /= 0) error stop 'problem closing group '//gname
  endif
end do

end subroutine hdf_add_group



!----- Helper functions

elemental function toLower(str)
! can be trivially extended to non-ASCII
character(*), intent(in) :: str
character(len(str)) :: toLower
character(*), parameter :: lower="abcdefghijklmnopqrstuvwxyz", &
                           upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
integer :: i,j

toLower = str

do concurrent (i = 1:len(str))
  j = index(upper,str(i:i))
  if (j > 0) toLower(i:i) = lower(j:j)
end do

end function toLower

end module hdf5_interface
