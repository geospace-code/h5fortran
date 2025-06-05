submodule (h5fortran:hdf5_read) h5f_datatype

implicit none

contains


module procedure get_class

integer(HID_T) :: dset_id
integer :: ier

call H5Dopen_f(self%file_id, dname, dset_id, ier)
call estop(ier, "get_class:H5Dopen", self%filename, dname)

call get_obj_class(self, dname, dset_id, get_class)

call H5Dclose_f(dset_id, ier)
call estop(ier, "get_class:H5Dclose", self%filename, dname)

end procedure get_class


module procedure get_obj_class

integer :: ier, obj_type
integer(HID_T) :: obj_dtype, native_dtype

call H5Iget_type_f(obj_id, obj_type, ier)
call estop(ier, "get_obj_class:H5Iget_type", self%filename, obj_name)

if(obj_type == H5I_DATASET_F) then
  call H5Dget_type_f(obj_id, obj_dtype, ier)
elseif(obj_type == H5I_ATTR_F) then
  call H5Aget_type_f(obj_id, obj_dtype, ier)
else
  error stop "ERROR:h5fortran:get_obj_class: only datasets and attributes have datatype " // obj_name // " " // self%filename
endif
call estop(ier, "get_obj_class:H5[A,D]get_type", self%filename, obj_name)

call H5Tget_native_type_f(obj_dtype, H5T_DIR_ASCEND_F, native_dtype, ier)
call estop(ier, "get_obj_class:H5Tget_native_type", self%filename, obj_name)

!> compose datatype inferred
call H5Tget_class_f(native_dtype, class, ier)
call estop(ier, "get_obj_class:H5Tget_class", self%filename, obj_name)

if(present(size_bytes)) then
  call H5Tget_size_f(native_dtype, size_bytes, ier)
  call estop(ier, "get_obj_class:H5Tget_size", self%filename, obj_name)
endif

call H5Tclose_f(native_dtype, ier)
call estop(ier, "get_obj_class:H5Tclose", self%filename, obj_name)

if(present(pad_type)) then
  if(class /= H5T_STRING_F) error stop "ERROR:h5fortran:get_class: pad_type only for string"

  call H5Tget_strpad_f(obj_dtype, pad_type, ier)
  call estop(ier, "get_obj_class:H5Tget_strpad", self%filename, obj_name)
endif

call H5Tclose_f(obj_dtype, ier)
call estop(ier, "get_obj_class:H5Tclose", self%filename, obj_name)

end procedure get_obj_class


module procedure get_native_dtype
!! get the dataset variable type:
!! {H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE}

integer :: class, ier
! integer :: order, machine_order
integer(size_t) :: size_bytes
integer(HID_T) :: o_id

if(present(obj_id)) then
  o_id = obj_id
else
  !! assume dataset
  call H5Dopen_f(self%file_id, dname, o_id, ier)
  call estop(ier, "get_native_dtype:H5Dopen", self%filename, dname)
endif

call get_obj_class(self, dname, o_id, class, size_bytes=size_bytes)

if(.not.present(obj_id)) then
  call H5Dclose_f(o_id, ier)
  call estop(ier, "get_native_dtype:H5Dclose", self%filename, dname)
endif

!> endianness and within type casting is handled by HDF5
! call h5tget_order_f(native_dtype, order, ier)
! if(ier/=0) error stop 'ERROR:h5fortran:reader: get endianness ' // dname // ' from ' // self%filename
! !> check dataset endianness matches machine (in future, could swap endianness if needed)
! call h5tget_order_f(H5T_NATIVE_INTEGER, machine_order, ier)
! if(order /= machine_order) error stop 'ERROR:h5fortran:read: endianness /= machine native: ' &
! // dname // ' from ' // self%filename

if(class == H5T_INTEGER_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_INTEGER
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_STD_I64LE
  else
    error stop "ERROR:h5fortran:get_native_dtype: expected 32-bit or 64-bit integer:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_FLOAT_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_REAL
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_NATIVE_DOUBLE
  else
    error stop "ERROR:h5fortran:get_native_dtype: expected 32-bit or 64-bit real:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_STRING_F) then
  get_native_dtype = H5T_NATIVE_CHARACTER
else
  error stop "ERROR:h5fortran:get_native_dtype: non-handled datatype: " // dname // " from " // self%filename
endif

end procedure get_native_dtype

end submodule h5f_datatype
