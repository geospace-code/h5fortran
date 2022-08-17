class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
integer(HSIZE_T), intent(in) :: attr_dims(rank(A)), dsize

TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
character(dsize), dimension(:), allocatable :: buf
CHARACTER(dsize, kind=C_CHAR), POINTER :: cstr
TYPE(C_PTR) :: f_ptr

integer :: ier
integer(HSIZE_T) :: i

allocate(cbuf(product(attr_dims)), buf(product(attr_dims)))
f_ptr = C_LOC(cbuf)

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

do i = 1, size(cbuf)
  call C_F_POINTER(cbuf(i), cstr)
  buf(i) = pad_trim(cstr)
enddo

A = reshape(buf, attr_dims)
