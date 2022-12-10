submodule (h5fortran:hdf5_read) read_scalar

use hdf5, only : H5Dread_f

implicit none


contains


module procedure h5read_scalar

integer(HSIZE_T) :: dims(0)
integer(HID_T) :: dset_id, xfer_id, file_space_id, mem_space_id
integer :: dclass, ier

logical :: is_scalar


call H5Dopen_f(self%file_id, dname, dset_id, ier)
call estop(ier, "read_scalar:H5Dopen", self%filename, dname)
call H5Dget_space_f(dset_id, file_space_id, ier)
call estop(ier, "read_scalar:H5Dget_space", self%filename, dname)

call hdf_rank_check(self, dname, file_space_id, rank(A), is_scalar)

if (is_scalar) then
  call hdf_get_slice(dims, dset_id, file_space_id, mem_space_id, [1], [1])
else
  call H5Dget_space_f(dset_id, mem_space_id, ier)
  call estop(ier, "read_scalar:H5Dget_space", self%filename, dname)
endif

call get_obj_class(self, dname, dset_id, dclass)

xfer_id = H5P_DEFAULT_F

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
!! https://support.hdfgroup.org/HDF5/doc/UG/HDF5_Users_Guide-Responsive%20HTML5/index.html#t=HDF5_Users_Guide%2FDatatypes%2FHDF5_Datatypes.htm%23TOC_6_10_Data_Transferbc-26&rhtocid=6.5_2
if(dclass == H5T_FLOAT_F .OR. dclass == H5T_INTEGER_F) then
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
    error stop 'ERROR:h5fortran:read: numeric dataset ' // dname // ' needs numeric memory variable'
  end select
  call estop(ier, "read_scalar:H5Dread", self%filename, dname)
elseif(dclass == H5T_STRING_F) then
  select type(A)
  type is (character(*)) !< kind=c_char too
    call read_char0(self, dname, A, dset_id, mem_space_id, file_space_id)
  class default
    error stop 'ERROR:h5fortran:read: string dataset ' // dname // ' needs character memory variable'
  end select
else
  error stop 'ERROR:h5fortran:reader: non-handled datatype--please reach out to developers.'
end if

call H5Dclose_f(dset_id, ier)
call estop(ier, "read_scalar:H5Dclose", self%filename, dname)

call H5Sclose_f(mem_space_id, ier)
call estop(ier, "read_scalar:H5Sclose", self%filename, dname)

call H5Sclose_f(file_space_id, ier)
call estop(ier, "read_scalar:H5Sclose", self%filename, dname)

end procedure h5read_scalar

end submodule read_scalar
