class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
integer, intent(in) :: obj_type
integer(HID_T), intent(in) :: obj_id, type_id, mem_space_id, file_space_id
integer(HSIZE_T), intent(in) :: dims(rank(A)), dsize

TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
character(dsize), dimension(:), allocatable :: buf
CHARACTER(dsize, kind=C_CHAR), POINTER :: cstr
TYPE(C_PTR) :: f_ptr

integer :: ier
integer(HSIZE_T) :: i

allocate(cbuf(product(dims)), buf(product(dims)))
f_ptr = C_LOC(cbuf)

if(obj_type == H5I_DATASET_F) then
  call H5Dread_f(obj_id, type_id, f_ptr, ier, mem_space_id, file_space_id)
elseif(obj_type == H5I_ATTR_F) then
  call H5Aread_f(obj_id, type_id, f_ptr, ier)
endif
if(ier/=0) error stop "h5fortran:read:read_ascii: read " // obj_name // " " // self%filename

do i = 1, size(cbuf)
  call C_F_POINTER(cbuf(i), cstr)
  buf(i) = pad_trim(cstr)
enddo

A = reshape(buf, dims)
