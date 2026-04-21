integer :: ier, attr_class
integer, parameter :: rA = rank(A)
integer(HID_T) :: attr_id, space_id
integer(HSIZE_T) :: attr_dims(rA)
logical :: is_scalar, ok

ok = .true.
attr_class = h5t_no_class_f !< in case not assigned

attr_dims = shape(A, HSIZE_T)

call H5Aopen_by_name_f(self%file_id, obj_name, attr_name, attr_id, ier)
call estop(ier, "attr_read:H5Aopen_by_name", self%filename, obj_name, attr_name, ok=ok)

if (ok) then
call H5Aget_space_f(attr_id, space_id, ier)
call estop(ier, "attr_read:H5Aget_space", self%filename, obj_name, attr_name, ok=ok)
endif

if (ok) then
if(rA == 0) then
  ok = hdf_rank_check(self, obj_name // ":" // attr_name, space_id, rA, is_scalar)
else
  ok = hdf_shape_check(self, obj_name // ":" // attr_name, space_id, attr_dims)
endif
endif

if (ok) ok = get_obj_class(self, obj_name // ":" // attr_name, attr_id, attr_class)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x, "y")
!! select case doesn't allow H5T_*
if(ok) then
if(any(attr_class == [H5T_FLOAT_F, H5T_INTEGER_F])) then
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
    write (stderr, '(a)') 'ERROR:h5fortran:readattr: numeric ' // obj_name // ':' // attr_name // ' needs real or integer variable'
    ok = .false.
  end select
  call estop(ier, "attr_read:H5Aread", self%filename, obj_name, attr_name, ok=ok)
elseif(attr_class == H5T_STRING_F) then
  select type(A)
  type is (character(*)) !< kind=c_char too
    call read_char(self, obj_name//":"//attr_name, A, attr_id, 0_HID_T, space_id)
    !! the "0_HID_T" is unused by H5Aread_F. GCC 7 doesn't like H5S_ALL_F, and it's unused anyway
    !! making this argument optional in read_char() caused problems with all compilers recognizing the TKR
  class default
    write(stderr, '(a)') 'ERROR:h5fortran:readattr: string ' // obj_name // ':' // attr_name // ' needs character memory variable'
    ok = .false.
  end select
else
  write(stderr, '(a)') "ERROR:h5fortran:readattr: unknown attribute type for " // obj_name // ':' // attr_name
  ok = .false.
endif
endif


call H5Aclose_f(attr_id, ier)
call estop(ier, "attr_read:H5Aclose", self%filename, obj_name, attr_name, ok=ok)

call H5Sclose_f(space_id, ier)
call estop(ier, "attr_read:H5Sclose", self%filename, obj_name, attr_name, ok=ok)

if(present(ierr)) then
  ierr = ier
elseif (.not. ok) then
  error stop "ERROR:h5fortran:attr_read"
endif
