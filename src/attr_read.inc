integer :: ier, attr_class
integer(HID_T) :: attr_id
integer(HSIZE_T) :: attr_dims(rank(A))

attr_dims = shape(A, HSIZE_T)

call attr_shape_check(self, obj_name, attr, attr_dims)

call get_attr_class(self, obj_name, attr, attr_class)

call H5Aopen_by_name_f(self%file_id, obj_name, attr, attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name: " // obj_name // ":" // attr // ":" // self%filename

!> cast the dataset read from disk to the variable type presented by user h5f%readattr("/my_dataset", x, "y")
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
if(attr_class == H5T_FLOAT_F) then
  select type(A)
  type is (real(real64))
    call H5Aread_f(attr_id, H5T_NATIVE_DOUBLE, A, attr_dims, ier)
  type is (real(real32))
    call H5Aread_f(attr_id, H5T_NATIVE_REAL, A, attr_dims, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: real disk dataset ' // obj_name // ':' // attr // ' needs real memory variable'
  end select
elseif(attr_class == H5T_INTEGER_F) then
  select type(A)
  type is (integer(int32))
    call H5Aread_f(attr_id, H5T_NATIVE_INTEGER, A, attr_dims, ier)
  type is (integer(int64))
    call H5Aread_f(attr_id, H5T_STD_I64LE, A, attr_dims, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: integer disk dataset ' // obj_name // ':' // attr // ' needs integer memory variable'
  end select
elseif(attr_class == H5T_STRING_F) then
  error stop "ERROR:h5fortran:readattr: attribute character arrays (non-singleton) not yet supported by h5fortran."
else
  error stop "ERROR:h5fortran:readattr: unknown attribute type for " // obj_name // ':' // attr
endif

if (ier /= 0) error stop "ERROR:h5fortran:readattr: " // obj_name // " in " // self%filename

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aclose: " // obj_name // ":" // attr // ":" // self%filename
