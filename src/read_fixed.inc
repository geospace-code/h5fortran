class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name
integer, intent(in) :: obj_type
integer(HID_T), intent(in) :: obj_id, type_id, mem_space_id, file_space_id

TYPE(C_PTR) :: f_ptr

integer :: ier

allocate(buf, mold=A)
f_ptr = C_LOC(buf)

if(obj_type == H5I_DATASET_F) then
  call H5Dread_f(obj_id, type_id, f_ptr, ier, mem_space_id, file_space_id)
elseif(obj_type == H5I_ATTR_F) then
  call H5Aread_f(obj_id, type_id, f_ptr, ier)
endif
call estop(ier, "read_fixed:H5[D,A]read", self%filename, obj_name)

A = pad_trim(buf)
