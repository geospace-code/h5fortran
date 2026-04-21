integer(HSIZE_T), dimension(rank(A)) :: dims
integer(HID_T) :: dset_id, file_space_id, mem_space_id, xfer_id
integer :: dclass, ier
logical :: ok

ok = .true.
xfer_id = H5P_DEFAULT_F !< can't be PARAMETER
dclass = h5t_no_class_f !< in case not assigned

dims = shape(A, HSIZE_T)

call h5dopen_f(self%file_id, dname, dset_id, ier)
call estop(ier, 'reader:H5Dopen', dname, self%filename, ok=ok)

if (ok) then
call H5Dget_space_f(dset_id, file_space_id, ier)
call estop(ier, 'reader:H5Dget_space', dname, self%filename)
endif

if(ok) then
if(present(istart) .and. present(iend)) then
  ok = hdf_get_slice(dims, dset_id, file_space_id, mem_space_id, istart=istart, iend=iend, stride=stride)
else
  ok = hdf_shape_check(self, dname, file_space_id, dims)

  if (ok) then
  call H5Dget_space_f(dset_id, mem_space_id, ier)
  call estop(ier, "reader:H5Dget_space", self%filename, dname, ok=ok)
  endif
endif
endif

if (ok) ok = get_obj_class(self, dname, dset_id, dclass)

!> casting is handled by HDF5 library internally
!! select case doesn't allow H5T_*
if(ok) then
if(any(dclass == [H5T_FLOAT_F, H5T_INTEGER_F])) then
  select type(A)
  type is (real(real64))
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (real(real32))
    call h5dread_f(dset_id, H5T_NATIVE_REAL, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (integer(int32))
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (integer(int64))
    call h5dread_f(dset_id, H5T_STD_I64LE, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  class default
    write(stderr,'(a)') 'ERROR:h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
    ok = .false.
  end select
  call estop(ier, "reader:H5Dread", self%filename, dname, ok=ok)
elseif(dclass == H5T_STRING_F) then
  select type(A)
  type is (character(*)) !< kind=c_char too
    call read_char(self, dname, A, dset_id, mem_space_id, file_space_id)
  class default
    write (stderr, '(a)') 'ERROR:h5fortran:read: string dataset ' // dname // ' needs character memory variable'
    ok = .false.
  end select
else
  write(stderr, '(a)') 'ERROR:h5fortran:reader: ' // dname // ' non-handled datatype--please reach out to developers.'
  ok = .false.
end if
endif


block
logical :: cok = .true.

call H5Dclose_f(dset_id, ier)
call estop(ier, "reader:H5Dclose", self%filename, dname, ok=cok)

call H5Sclose_f(mem_space_id, ier)
call estop(ier, "reader:H5Sclose", self%filename, dname, ok=cok)

call H5Sclose_f(file_space_id, ier)
call estop(ier, "reader:H5Sclose", self%filename, dname, ok=cok)

if (ok) ok = cok
endblock

if(present(ierr)) then
  ierr = ier
elseif (.not. ok) then
  error stop 'ERROR:h5fortran:read: failed to read dataset ' // dname
end if
