submodule (h5fortran:hdf5_read) read_scalar

use hdf5, only : H5Dread_f

implicit none


contains


module procedure h5read_scalar

integer(HSIZE_T) :: dims(0)
integer(HID_T) :: dset_id, xfer_id, file_space_id, mem_space_id
integer :: dclass, ier

logical :: is_scalar, ok

ok = .true.
xfer_id = H5P_DEFAULT_F !< can't be PARAMETER
dclass = h5t_no_class_f !< in case not assigned

call H5Dopen_f(self%file_id, dname, dset_id, ier)
call estop(ier, "read_scalar:H5Dopen", self%filename, dname, ok=ok)

if(ok) then
call H5Dget_space_f(dset_id, file_space_id, ier)
call estop(ier, "read_scalar:H5Dget_space", self%filename, dname, ok=ok)
endif

if (ok) ok = hdf_rank_check(self, dname, file_space_id, 0, is_scalar)

if(ok) then
if (is_scalar) then
  ok = hdf_get_slice(dims, dset_id, file_space_id, mem_space_id, [1], [1])
else
  call H5Dget_space_f(dset_id, mem_space_id, ier)
  call estop(ier, "read_scalar:H5Dget_space", self%filename, dname, ok=ok)
endif
endif

if (ok) ok = get_obj_class(self, dname, dset_id, dclass)


!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
!! https://support.hdfgroup.org/HDF5/doc/UG/HDF5_Users_Guide-Responsive%20HTML5/index.html#t=HDF5_Users_Guide%2FDatatypes%2FHDF5_Datatypes.htm%23TOC_6_10_Data_Transferbc-26&rhtocid=6.5_2
if(any(dclass == [H5T_FLOAT_F, H5T_INTEGER_F])) then
  select type(A)
  type is (real(real64))
    call H5Dread_f(dset_id, H5T_NATIVE_DOUBLE, A, dims, ier, mem_space_id, file_space_id)
  type is (real(real32))
    call H5Dread_f(dset_id, H5T_NATIVE_REAL, A, dims, ier, mem_space_id, file_space_id)
  type is (integer(int32))
    call H5Dread_f(dset_id, H5T_NATIVE_INTEGER, A, dims, ier, mem_space_id, file_space_id)
  type is (integer(int64))
    call H5Dread_f(dset_id, H5T_STD_I64LE, A, dims, ier, mem_space_id, file_space_id)
  class default
    write(stderr, '(a)') 'ERROR:h5fortran:read: numeric dataset ' // dname // ' needs numeric memory variable'
    ok = .false.
  end select
  call estop(ier, "read_scalar:H5Dread", self%filename, dname, ok=ok)
elseif(dclass == H5T_STRING_F) then
  select type(A)
  type is (character(*)) !< kind=c_char too
    call read_char0(self, dname, A, dset_id, mem_space_id, file_space_id)
  class default
    write(stderr, '(a)') 'ERROR:h5fortran:read: string dataset ' // dname // ' needs character memory variable'
    ok = .false.
  end select
else
  write (stderr, '(a)') 'ERROR:h5fortran:reader: ' // dname // ' non-handled datatype--please reach out to developers.'
  ok = .false.
end if

call H5Dclose_f(dset_id, ier)
call estop(ier, "read_scalar:H5Dclose", self%filename, dname, ok=ok)

call H5Sclose_f(mem_space_id, ier)
call estop(ier, "read_scalar:H5Sclose", self%filename, dname, ok=ok)

call H5Sclose_f(file_space_id, ier)
call estop(ier, "read_scalar:H5Sclose", self%filename, dname, ok=ok)

if(present(ierr)) then
  ierr = ier
elseif (.not. ok) then
  error stop 'ERROR:h5fortran:read: failed to read dataset ' // dname
end if

end procedure h5read_scalar

end submodule read_scalar
