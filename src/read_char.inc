integer(HID_T) :: type_id
integer :: ier, charlen, obj_type
logical :: is_vlen

!> variable length string
integer(HSIZE_T) :: dims(rank(A)), Npts, dsize

charlen = len(A)

call open_char(self, obj_name, obj_id, file_space_id, charlen, obj_type, type_id, dims, Npts, dsize, is_vlen)

if(is_vlen) then
  call read_vlen(self, obj_name, obj_type, obj_id, type_id, dims, dsize, A, mem_space_id, file_space_id)
else
  call read_fixed(self, obj_name, obj_type, obj_id, type_id, dims, dsize, A, mem_space_id, file_space_id)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name
