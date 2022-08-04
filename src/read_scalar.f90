submodule (h5fortran:hdf5_read) read_scalar

use hdf5, only : H5Dread_f, &
H5Sclose_f

implicit none (type, external)

interface
module subroutine read_scalar_char(A, dset_id, file_space_id, mem_space_id, dims)
class(*), intent(inout) :: A
integer(HID_T), intent(in) :: dset_id, file_space_id
integer(HID_T), intent(inout) :: mem_space_id
integer(HSIZE_T), intent(in) :: dims(:)
end subroutine
end interface

contains


module procedure h5read_scalar

integer(HSIZE_T) :: dims(0)
integer(HID_T) :: dset_id, file_space_id, mem_space_id
integer :: dclass, ier

logical :: is_scalar


call H5Dopen_f(self%file_id, dname, dset_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename
call H5Dget_space_f(dset_id, file_space_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:reader:H5Dget_space ' // dname // ' from ' // self%filename

call hdf_rank_check(self, dname, rank(A), is_scalar)

if (is_scalar) then
  call hdf_get_slice(self, dname, dset_id, file_space_id, mem_space_id, [1], [1])
else
  mem_space_id = H5S_ALL_F
endif

call get_dset_class(self, dname, dclass, dset_id)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
!! https://support.hdfgroup.org/HDF5/doc/UG/HDF5_Users_Guide-Responsive%20HTML5/index.html#t=HDF5_Users_Guide%2FDatatypes%2FHDF5_Datatypes.htm%23TOC_6_10_Data_Transferbc-26&rhtocid=6.5_2
if(dclass == H5T_FLOAT_F) then
  select type(A)
  type is (real(real64))
    call H5Dread_f(dset_id, H5T_NATIVE_DOUBLE, A, dims, ier)
  type is (real(real32))
    call H5Dread_f(dset_id, H5T_NATIVE_REAL, A, dims, ier)
  class default
    error stop 'ERROR:h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(dclass == H5T_INTEGER_F) then
  select type(A)
  type is (integer(int32))
    call H5Dread_f(dset_id, H5T_NATIVE_INTEGER, A, dims, ier)
  type is (integer(int64))
    call H5Dread_f(dset_id, H5T_STD_I64LE, A, dims, ier)
  class default
    error stop 'ERROR:h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
elseif(dclass == H5T_STRING_F) then
  call read_scalar_char(A, dset_id, file_space_id, mem_space_id, dims)
else
  error stop 'ERROR:h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'ERROR:h5fortran:reader: reading ' // dname // ' from ' // self%filename

call H5Dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:read_scalar: closing dataset: " // dname // " in " // self%filename

if(mem_space_id /= H5S_ALL_F) call H5Sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:read_scalar closing memory dataspace: " // dname // " in " // self%filename

call H5Sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:read_scalar closing file dataspace: " // dname // " in " // self%filename

end procedure h5read_scalar

end submodule read_scalar
