submodule (h5fortran:attr_read) attr_read_char

implicit none (type, external)

contains


module procedure open_attr_char

integer :: drank, ier
integer(HSIZE_T), dimension(size(attr_dims)) :: maxdims

call H5Aget_type_f(attr_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_type " // obj_name // ":" // attr_name // " " // self%filename

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


subroutine readattr_char_scalar_vlen(self, obj_name, attr_name, attr_id, type_id, Npts, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
integer(HSIZE_T), intent(in) :: Npts
character(*), intent(inout) :: A

TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
TYPE(C_PTR) :: f_ptr

CHARACTER(10000, kind=c_char), POINTER :: cstr !< arbitrary maximum variable length string

integer :: ier, i, L

L = len(A)

allocate(cbuf(Npts))
f_ptr = C_LOC(cbuf)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

call C_F_POINTER(cbuf(1), cstr)

i = index(cstr, c_null_char) - 1
if (i == -1) i = len_trim(cstr)
if(i > L) then
  write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr_vlen: buffer too small: ", &
    i, " > ", L, obj_name // ":" // attr_name // " " // self%filename
  error stop
endif

A = cstr(:i)

end subroutine readattr_char_scalar_vlen


subroutine readattr_char0_fixed(self, obj_name, attr_name, attr_id, type_id, Npts, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
integer(HSIZE_T), intent(in) :: Npts
character(*), intent(inout) :: A

TYPE(C_PTR) :: f_ptr
integer(HSIZE_T) :: dsize

CHARACTER(:), DIMENSION(:), ALLOCATABLE, TARGET :: buf_char

integer :: i, L, ier

L = len(A)

call H5Tget_size_f(type_id, dsize, ier)
if(ier/=0) error stop "ERROR:h5fortran:attr_read:H5Tget_size " // obj_name // ":" // attr_name // " " // self%filename

if(dsize > L) then
  write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", dsize, " > ", L, obj_name // ":" // attr_name
  error stop
endif

allocate(character(dsize) :: buf_char(Npts))

f_ptr = C_LOC(buf_char)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // obj_name // ":" // attr_name

i = index(buf_char(1), c_null_char) - 1
if (i == -1) i = len_trim(buf_char(1))

A = buf_char(1)(:i)

end subroutine readattr_char0_fixed


subroutine readattr_char1_fixed(self, obj_name, attr_name, attr_id, type_id, attr_dims, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
integer(HSIZE_T), intent(in) :: attr_dims(1)
character(*), intent(inout) :: A(:)

TYPE(C_PTR) :: f_ptr
integer(HSIZE_T) :: dsize

CHARACTER(:), DIMENSION(:), ALLOCATABLE, TARGET :: buf_char

integer :: i, L, ier

L = len(A)

call H5Tget_size_f(type_id, dsize, ier)
if(ier/=0) error stop "ERROR:h5fortran:attr_read:H5Tget_size " // obj_name // ":" // attr_name // " " // self%filename

if(dsize > L) then
  write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", dsize, " > ", L, obj_name // ":" // attr_name
  error stop
endif

allocate(character(dsize) :: buf_char(attr_dims(1)))

! print *, "TRACE: ", obj_name,":",attr_name, " allocated Npts = ", Npts, "shape ", shape(buf_char), "len: ", len(buf_char)

f_ptr = C_LOC(buf_char)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // obj_name // ":" // attr_name

A = buf_char

end subroutine readattr_char1_fixed


module procedure readattr_char_scalar

integer(HSIZE_T) :: attr_dims(1), Npts
integer(HID_T) :: type_id
integer :: ier

logical :: is_vlen


call open_attr_char(self, obj_name, attr_name, attr_id, space_id, type_id, attr_dims, Npts)

call H5Tis_variable_str_f(type_id, is_vlen, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Tis_variable_str " // obj_name // ":" // attr_name

if(is_vlen) then
  call readattr_char_scalar_vlen(self, obj_name, attr_name, attr_id, type_id, Npts, A)
else
  call readattr_char0_fixed(self, obj_name, attr_name, attr_id, type_id, Npts, A)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name // ":" // attr_name

end procedure readattr_char_scalar


module procedure readattr_char1

integer(HSIZE_T) :: attr_dims(1), Npts
integer(HID_T) :: type_id
integer :: ier

logical :: is_vlen


call open_attr_char(self, obj_name, attr_name, attr_id, space_id, type_id, attr_dims, Npts)

call H5Tis_variable_str_f(type_id, is_vlen, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Tis_variable_str " // obj_name // ":" // attr_name

if(is_vlen) then
  error stop "readattr_char1: variable length strings not supported"
  ! call readattr_char1_vlen(self, obj_name, attr_name, attr_id, type_id, Npts, A)
else
  call readattr_char1_fixed(self, obj_name, attr_name, attr_id, type_id, attr_dims, A)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name // ":" // attr_name

end procedure readattr_char1

end submodule attr_read_char
