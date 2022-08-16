integer :: ier, charlen
integer(HID_T) :: dtype, attr_id, dtype_id
integer(HSIZE_T) :: attr_dims(rank(A))

if(.not.self%is_open()) error stop 'ERROR:h5fortran:writeattr: file handle is not open'

attr_dims = shape(A, HSIZE_T)

charlen = 0

select type(A)
type is (real(real32))
  dtype = H5T_NATIVE_REAL
type is (real(real64))
  dtype = H5T_NATIVE_DOUBLE
type is (integer(int32))
  dtype = H5T_NATIVE_INTEGER
type is (integer(int64))
  dtype = H5T_STD_I64LE
type is(character(*))
  dtype = H5T_NATIVE_CHARACTER
  charlen = len(A)  !< workaround for GCC 8.3.0 bug
class default
  error stop "ERROR:h5fortran:writeattr: unknown dataset type for " // obj_name // ":" // attr // " in " // self%filename
end select

call attr_create(self, obj_name, attr, dtype, attr_dims, attr_id, dtype_id, charlen=charlen)

select type(A)
type is (real(real32))
  call H5Awrite_f(attr_id, dtype_id, A, attr_dims, ier)
type is (real(real64))
  call H5Awrite_f(attr_id, dtype_id, A, attr_dims, ier)
type is (integer(int32))
  call H5Awrite_f(attr_id, dtype_id, A, attr_dims, ier)
type is (integer(int64))
  call H5Awrite_f(attr_id, dtype_id, A, attr_dims, ier)
type is(character(*))
  call H5Awrite_f(attr_id, dtype_id, A, attr_dims, ier)
class default
  error stop "ERROR:h5fortran:writeattr: unknown dataset type for " // obj_name // ":" // attr // " in " // self%filename
end select
if (ier /= 0) error stop 'ERROR:h5fortran:writeattr:H5Awrite: ' // obj_name // ":" // attr // " in " // self%filename

call H5Tclose_f(dtype_id, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:writeattr:H5Tclose ' // obj_name // ":" // attr // " in " // self%filename

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writeattr:H5Aclose " // obj_name // ":" // attr // " in " // self%filename
