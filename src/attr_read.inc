integer :: ier, attr_class
integer(HID_T) :: attr_id, space_id
integer(HSIZE_T) :: attr_dims(rank(A))
logical :: is_scalar

attr_dims = shape(A, HSIZE_T)

call H5Aopen_by_name_f(self%file_id, obj_name, attr_name, attr_id, ier)
call estop(ier, "attr_read:H5Aopen_by_name", self%filename, obj_name, attr_name)

call H5Aget_space_f(attr_id, space_id, ier)
call estop(ier, "attr_read:H5Aget_space", self%filename, obj_name, attr_name)

if(rank(A) == 0) then
  call hdf_rank_check(self, obj_name // ":" // attr_name, space_id, rank(A), is_scalar)
else
  call hdf_shape_check(self, obj_name // ":" // attr_name, space_id, attr_dims)
endif

call get_obj_class(self, obj_name // ":" // attr_name, attr_id, attr_class)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x, "y")
!! select case doesn't allow H5T_*
if(attr_class == H5T_FLOAT_F .OR. attr_class == H5T_INTEGER_F) then
  select type(A)
  type is (real(real64))
    call H5Aread_f(attr_id, H5T_NATIVE_DOUBLE, A, attr_dims, ier)
  type is (real(real32))
    call H5Aread_f(attr_id, H5T_NATIVE_REAL, A, attr_dims, ier)
  type is (integer(int32))
    call H5Aread_f(attr_id, H5T_NATIVE_INTEGER, A, attr_dims, ier)
  type is (integer(int64))
    call H5Aread_f(attr_id, H5T_STD_I64LE, A, attr_dims, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: numeric dataset ' // obj_name // ':' // attr_name // ' needs real or integer variable'
  end select
  call estop(ier, "attr_read:H5Aread", self%filename, obj_name, attr_name)
elseif(attr_class == H5T_STRING_F) then
  select type(A)
  type is (character(*)) !< kind=c_char too
    call read_char(self, obj_name//":"//attr_name, A, attr_id, H5S_ALL_F, space_id)
  class default
    error stop 'ERROR:h5fortran:readattr: string dataset ' // obj_name // ':' // attr_name // ' needs character memory variable'
  end select
else
  error stop "ERROR:h5fortran:readattr: unknown attribute type for " // obj_name // ':' // attr_name
endif


call H5Aclose_f(attr_id, ier)
call estop(ier, "attr_read:H5Aclose", self%filename, obj_name, attr_name)

call H5Sclose_f(space_id, ier)
call estop(ier, "attr_read:H5Sclose", self%filename, obj_name, attr_name)
