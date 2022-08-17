submodule (h5fortran:attr_read) attr_read_char

implicit none (type, external)

contains


module procedure open_attr_char

integer :: drank, ier
integer(HSIZE_T), dimension(size(attr_dims)) :: maxdims

call H5Aget_type_f(attr_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_type " // obj_name // ":" // attr_name // " " // self%filename

call H5Tis_variable_str_f(type_id, is_vlen, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Tis_variable_str " // obj_name // ":" // attr_name

call H5Tget_size_f(type_id, dsize, ier)
if(ier/=0) error stop "ERROR:h5fortran:attr_read:H5Tget_size " // obj_name // ":" // attr_name // " " // self%filename

if(.not. is_vlen .and. dsize > charlen) then
  write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", dsize, " > ", charlen, obj_name//":"//attr_name
  error stop
endif

CALL H5Sget_simple_extent_ndims_f(space_id, drank, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Sget_simple_extent_ndims " // obj_name // ":" // attr_name

call H5Sget_simple_extent_npoints_f(space_id, Npts, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:readattr:H5Sget_simple_extent_npoints: ' // obj_name // ":" // attr_name

if(drank > 0) then
  CALL H5Sget_simple_extent_dims_f(space_id, attr_dims, maxdims, ier)
  if(ier /= drank) error stop "ERROR:h5fortran:readattr:H5Sget_simple_extent_dims " // obj_name // ":" // attr_name
else
  attr_dims(1) = Npts
endif

end procedure open_attr_char


subroutine readattr_char0_vlen(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
integer(HSIZE_T), intent(in) :: attr_dims(1), dsize
character(*), intent(inout) :: A

TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
CHARACTER(dsize, kind=c_char), POINTER :: cstr
TYPE(C_PTR) :: f_ptr

integer :: ier

allocate(cbuf(attr_dims(1)))
f_ptr = C_LOC(cbuf)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

call C_F_POINTER(cbuf(1), cstr)

A = pad_trim(cstr)

end subroutine readattr_char0_vlen


subroutine readattr_char1_vlen(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
character(*), intent(inout), dimension(:) :: A
integer(HSIZE_T), intent(in) :: attr_dims(rank(A)), dsize

TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
CHARACTER(dsize, kind=C_CHAR), POINTER :: cstr
TYPE(C_PTR) :: f_ptr

integer :: ier
integer(HSIZE_T) :: j

allocate(cbuf(attr_dims(1)))
f_ptr = C_LOC(cbuf)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

do j = 1, attr_dims(1)
  call C_F_POINTER(cbuf(j), cstr)
  A(j) = pad_trim(cstr)
enddo

end subroutine readattr_char1_vlen


subroutine readattr_char2_vlen(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
character(*), intent(inout), dimension(:,:) :: A
integer(HSIZE_T), intent(in) :: attr_dims(rank(A)), dsize

TYPE(C_PTR), DIMENSION(:,:), ALLOCATABLE, TARGET :: cbuf
CHARACTER(dsize, kind=C_CHAR), POINTER :: cstr
TYPE(C_PTR) :: f_ptr

integer :: ier
integer(HSIZE_T) :: j, k

allocate(cbuf(attr_dims(1), attr_dims(2)))
f_ptr = C_LOC(cbuf)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

do k = 1, attr_dims(2)
  do j = 1, attr_dims(1)
    call C_F_POINTER(cbuf(j, k), cstr)
    A(j, k) = pad_trim(cstr)
  enddo
enddo

end subroutine readattr_char2_vlen


subroutine readattr_char0_fixed(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
integer(HSIZE_T), intent(in) :: attr_dims(1), dsize
character(*), intent(inout) :: A

TYPE(C_PTR) :: f_ptr
CHARACTER(:), DIMENSION(:), ALLOCATABLE, TARGET :: buf

integer :: ier

allocate(character(dsize) :: buf(attr_dims(1)))

f_ptr = C_LOC(buf)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

A = pad_trim(buf(1))

end subroutine readattr_char0_fixed


subroutine readattr_char1_fixed(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
character(*), intent(inout), dimension(:) :: A
integer(HSIZE_T), intent(in) :: attr_dims(rank(A)), dsize

TYPE(C_PTR) :: f_ptr
CHARACTER(:), DIMENSION(:), ALLOCATABLE, TARGET :: buf

integer :: ier

allocate(character(dsize) :: buf(attr_dims(1)))

! print *, "TRACE: ", obj_name,":",attr_name, " allocated attr_dims = ", attr_dims, "shape ", shape(buf), "len: ", len(buf)

f_ptr = C_LOC(buf)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

A = pad_trim(buf)

end subroutine readattr_char1_fixed


subroutine readattr_char2_fixed(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
character(*), intent(inout), dimension(:,:) :: A
integer(HSIZE_T), intent(in) :: attr_dims(rank(A)), dsize

TYPE(C_PTR) :: f_ptr
CHARACTER(:), DIMENSION(:,:), ALLOCATABLE, TARGET :: buf

integer :: ier

allocate(character(dsize) :: buf(attr_dims(1), attr_dims(2)))

! print *, "TRACE: ", obj_name,":",attr_name, " allocated attr_dims = ", attr_dims, "shape ", shape(buf), "len: ", len(buf)

f_ptr = C_LOC(buf)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

A = pad_trim(buf)

end subroutine readattr_char2_fixed


module procedure readattr_char_scalar

integer(HSIZE_T) :: attr_dims(1), Npts, dsize
integer(HID_T) :: type_id
integer :: ier, charlen
logical :: is_vlen

charlen = len(A)

call open_attr_char(self, obj_name, attr_name, attr_id, space_id, charlen, type_id, attr_dims, Npts, dsize, is_vlen)

if(is_vlen) then
  call readattr_char0_vlen(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)
else
  call readattr_char0_fixed(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name // ":" // attr_name

end procedure readattr_char_scalar


module procedure readattr_char1

integer(HSIZE_T) :: attr_dims(rank(A)), Npts, dsize
integer(HID_T) :: type_id
integer :: ier, charlen
logical :: is_vlen

charlen = len(A)

call open_attr_char(self, obj_name, attr_name, attr_id, space_id, charlen, type_id, attr_dims, Npts, dsize, is_vlen)

if(is_vlen) then
  call readattr_char1_vlen(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)
else
  call readattr_char1_fixed(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name // ":" // attr_name

end procedure readattr_char1


module procedure readattr_char2

integer(HSIZE_T) :: attr_dims(rank(A)), Npts, dsize
integer(HID_T) :: type_id
integer :: ier, charlen
logical :: is_vlen

charlen = len(A)

call open_attr_char(self, obj_name, attr_name, attr_id, space_id, charlen, type_id, attr_dims, Npts, dsize, is_vlen)

if(is_vlen) then
  call readattr_char2_vlen(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)
else
  call readattr_char2_fixed(self, obj_name, attr_name, attr_id, type_id, attr_dims, dsize, A)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name // ":" // attr_name

end procedure readattr_char2


elemental function pad_trim(s) result(t)
!! trim string for nullpad or spacepad
character(*), intent(in) :: s
character(len(s)) :: t

integer :: i

i = index(s, C_NULL_CHAR) - 1
if (i < 0) i = len_trim(s)
if (i == 0) i = 1

t = s(:i)

end function pad_trim


end submodule attr_read_char
