submodule (h5fortran:read_scalar) read_scalar_ascii

use, intrinsic :: iso_c_binding, only : C_CHAR, C_F_POINTER
use hdf5, only : H5T_STR_NULLTERM_F, &
H5Aread_f

implicit none (type, external)

interface read_vlen
procedure read_vlen0, read_vlen1, read_vlen2, read_vlen3, read_vlen4, read_vlen5, read_vlen6, read_vlen7
end interface

interface read_fixed
procedure read_fixed0, read_fixed1, read_fixed2, read_fixed3, read_fixed4, read_fixed5, read_fixed6, read_fixed7
end interface

contains

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
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name

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
if(ier/=0) error stop "ERROR:h5fortran:read_ascii: read " // obj_name // " " // self%filename

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
if(ier/=0) error stop "h5fortran:read:read_ascii: read " // obj_name // " " // self%filename

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
