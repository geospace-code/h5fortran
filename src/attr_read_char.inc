class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, space_id

integer(HSIZE_T) :: attr_dims(rank(A)), Npts, dsize
integer(HID_T) :: type_id
logical :: is_vlen
integer :: charlen

error stop "ERROR:h5fortran:readattr: attribute character arrays (non-singleton) not yet supported by h5fortran."

charlen = len(A)
call open_attr_char(self, obj_name, attr_name, attr_id, space_id, charlen, type_id, attr_dims, Npts, dsize, is_vlen)
