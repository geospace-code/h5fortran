submodule (h5fortran:read_scalar) read_scalar_ascii

use, intrinsic :: iso_c_binding, only : C_CHAR, C_F_POINTER
use hdf5, only : H5T_STR_NULLTERM_F, &
H5Aread_f

implicit none

interface read_vlen
procedure read_vlen0, read_vlen1, read_vlen2, read_vlen3, read_vlen4, read_vlen5, read_vlen6, read_vlen7
end interface

interface read_fixed
procedure read_fixed0, read_fixed1, read_fixed2, read_fixed3, read_fixed4, read_fixed5, read_fixed6, read_fixed7
end interface

contains

subroutine open_char(self, obj_name, obj_id, space_id, charlen, obj_type, type_id, dims, Npts, dsize, is_vlen)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
integer(HID_T), intent(in) :: obj_id, space_id
integer, intent(in) :: charlen
integer, intent(out) :: obj_type
integer(HID_T), intent(out) :: type_id
integer(HSIZE_T), intent(out) :: dims(:), Npts, dsize
logical, intent(out) :: is_vlen

integer :: drank, ier
integer(HSIZE_T), dimension(size(dims)) :: maxdims
integer(SIZE_T) :: ds_size

call H5Iget_type_f(obj_id, obj_type, ier)
call estop(ier, "open_char:H5Iget_type", self%filename, obj_name)

if(obj_type == H5I_DATASET_F) then
  call H5Dget_type_f(obj_id, type_id, ier)
elseif(obj_type == H5I_ATTR_F) then
  call H5Aget_type_f(obj_id, type_id, ier)
else
  error stop "ERROR:h5fortran:read: only datasets and attributes have datatype " // obj_name // " " // self%filename
endif
call estop(ier, "open_char:H5[D,A]get_type", self%filename, obj_name)


call H5Tis_variable_str_f(type_id, is_vlen, ier)
call estop(ier, "open_char:H5Tis_variable", self%filename, obj_name)

if(is_vlen) then
  if(obj_type == H5I_DATASET_F) then
    call H5Dget_storage_size_f(obj_id, dsize, ier)
  elseif(obj_type == H5I_ATTR_F) then
    call H5Aget_storage_size_f(obj_id, dsize, ier)
  endif
  call estop(ier, "open_char:H5[D,a]get_storage_size", self%filename, obj_name)
else
  call H5Tget_size_f(type_id, ds_size, ier)
  call estop(ier, "open_char:H5Tget_size", self%filename, obj_name)

  if(ds_size > charlen) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:read: buffer too small: ", ds_size, " > ", charlen, obj_name
    error stop
  endif

  dsize = int(ds_size, HSIZE_T)
endif


CALL H5Sget_simple_extent_ndims_f(space_id, drank, ier)
call estop(ier, "open_char:H5Sget_simple_extent_ndims", self%filename, obj_name)

call H5Sget_simple_extent_npoints_f(space_id, Npts, ier)
call estop(ier, "open_char:H5Sget_simple_extent_npoints", self%filename, obj_name)

if(drank > 0) then
  CALL H5Sget_simple_extent_dims_f(space_id, dims, maxdims, ier)
  if(ier /= drank) error stop "ERROR:h5fortran:read:H5Sget_simple_extent_dims " // obj_name // " " // self%filename
else
  dims(1) = Npts
endif

end subroutine open_char


elemental function pad_trim(s) result(t)
!! trim string for nullpad or spacepad
character(*), intent(in) :: s
character(len(s)) :: t
integer :: i

i = index(s, C_NULL_CHAR) - 1
if (i < 0) i = len_trim(s)

if (i > 0) then
  t = s(1:i)
else
  t = ""
endif

end function pad_trim


module procedure read_char0

integer(HID_T) :: type_id
integer :: ier, charlen, obj_type
logical :: is_vlen

!> variable length string
integer(HSIZE_T) :: dims(1), Npts, dsize

charlen = len(A)

call open_char(self, obj_name, obj_id, file_space_id, charlen, obj_type, type_id, dims, Npts, dsize, is_vlen)

if(is_vlen) then
  call read_vlen(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
else
  call read_fixed(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
endif

call H5Tclose_f(type_id, ier)
call estop(ier, "read_char0:H5Tclose", self%filename, obj_name)

end procedure read_char0

module procedure read_char1
include 'read_char.inc'
end procedure

module procedure read_char2
include 'read_char.inc'
end procedure

module procedure read_char3
include 'read_char.inc'
end procedure

module procedure read_char4
include 'read_char.inc'
end procedure

module procedure read_char5
include 'read_char.inc'
end procedure

module procedure read_char6
include 'read_char.inc'
end procedure

module procedure read_char7
include 'read_char.inc'
end procedure


subroutine read_fixed0(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
integer, intent(in) :: obj_type
integer(HID_T), intent(in) :: obj_id, type_id, mem_space_id, file_space_id
integer(HSIZE_T), intent(in) :: dsize
character(*), intent(inout) :: A

TYPE(C_PTR) :: f_ptr
CHARACTER(len=dsize), DIMENSION(:), ALLOCATABLE, TARGET :: buf

integer :: ier

allocate(buf(1))

f_ptr = C_LOC(buf)

if(obj_type == H5I_DATASET_F) then
  call H5Dread_f(obj_id, type_id, f_ptr, ier, mem_space_id, file_space_id)
elseif(obj_type == H5I_ATTR_F) then
  call H5Aread_f(obj_id, type_id, f_ptr, ier)
endif
call estop(ier, "read_fixed0:H5[D,A]read", self%filename, obj_name)

A = pad_trim(buf(1))

end subroutine read_fixed0

subroutine read_fixed1(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:) :: A
integer(HSIZE_T), intent(in) :: dsize
CHARACTER(len=dsize), DIMENSION(:), ALLOCATABLE, TARGET :: buf
include 'read_fixed.inc'
end subroutine

subroutine read_fixed2(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:) :: A
integer(HSIZE_T), intent(in) :: dsize
CHARACTER(len=dsize), DIMENSION(:,:), ALLOCATABLE, TARGET :: buf
include 'read_fixed.inc'
end subroutine


subroutine read_fixed3(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:) :: A
integer(HSIZE_T), intent(in) :: dsize
CHARACTER(len=dsize), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: buf
include 'read_fixed.inc'
end subroutine

subroutine read_fixed4(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:,:) :: A
integer(HSIZE_T), intent(in) :: dsize
CHARACTER(len=dsize), DIMENSION(:,:,:,:), ALLOCATABLE, TARGET :: buf
include 'read_fixed.inc'
end subroutine

subroutine read_fixed5(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:,:,:) :: A
integer(HSIZE_T), intent(in) :: dsize
CHARACTER(len=dsize), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET :: buf
include 'read_fixed.inc'
end subroutine

subroutine read_fixed6(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:,:,:,:) :: A
integer(HSIZE_T), intent(in) :: dsize
CHARACTER(len=dsize), DIMENSION(:,:,:,:,:,:), ALLOCATABLE, TARGET :: buf
include 'read_fixed.inc'
end subroutine

subroutine read_fixed7(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:,:,:,:,:) :: A
integer(HSIZE_T), intent(in) :: dsize
CHARACTER(len=dsize), DIMENSION(:,:,:,:,:,:,:), ALLOCATABLE, TARGET :: buf
include 'read_fixed.inc'
end subroutine


subroutine read_vlen0(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
integer, intent(in) :: obj_type
integer(HID_T), intent(in) :: obj_id, type_id, mem_space_id, file_space_id
integer(HSIZE_T), intent(in) :: dsize
character(*), intent(inout) :: A

TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
CHARACTER(dsize, kind=c_char), POINTER :: cstr
TYPE(C_PTR) :: f_ptr

integer :: ier

allocate(cbuf(1))
f_ptr = C_LOC(cbuf)

if(obj_type == H5I_DATASET_F) then
  call H5Dread_f(obj_id, type_id, f_ptr, ier, mem_space_id, file_space_id)
elseif(obj_type == H5I_ATTR_F) then
  call H5Aread_f(obj_id, type_id, f_ptr, ier)
endif
call estop(ier, "open_char:H5[D,A]read", self%filename, obj_name)

call C_F_POINTER(cbuf(1), cstr)

A = pad_trim(cstr)

end subroutine read_vlen0

subroutine read_vlen1(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:) :: A
include 'read_vlen.inc'
end subroutine

subroutine read_vlen2(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:) :: A
include 'read_vlen.inc'
end subroutine

subroutine read_vlen3(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:) :: A
include 'read_vlen.inc'
end subroutine

subroutine read_vlen4(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:,:) :: A
include 'read_vlen.inc'
end subroutine

subroutine read_vlen5(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:,:,:) :: A
include 'read_vlen.inc'
end subroutine

subroutine read_vlen6(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:,:,:,:) :: A
include 'read_vlen.inc'
end subroutine

subroutine read_vlen7(self, obj_name, obj_type, obj_id, type_id, dsize, A, mem_space_id, file_space_id)
character(*), intent(inout), dimension(:,:,:,:,:,:,:) :: A
include 'read_vlen.inc'
end subroutine

end submodule read_scalar_ascii
