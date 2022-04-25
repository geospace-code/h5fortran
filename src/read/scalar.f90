submodule (h5fortran:hdf5_read) read_scalar

use, intrinsic :: iso_c_binding, only : c_null_char
use h5lt, only : h5ltread_dataset_string_f
use hdf5, only : h5dread_f

implicit none (type, external)

contains

module procedure h5read_scalar

integer(HSIZE_T) :: dims(rank(value))
integer(SIZE_T) :: dsize
integer(hid_t) :: dset_id, type_id
integer :: dclass, ier

logical :: vector_scalar

real(real32) :: buf_r32(1)
real(real64) :: buf_r64(1)
integer(int32) :: buf_i32(1)
integer(int64) :: buf_i64(1)

call hdf_rank_check(self, dname, rank(value), vector_scalar)
if(vector_scalar) then
  select type(value)
  type is (real(real32))
    call h5read_1d(self, dname, buf_r32)
    value = buf_r32(1)
  type is (real(real64))
    call h5read_1d(self, dname, buf_r64)
    value = buf_r64(1)
  type is (integer(int32))
    call h5read_1d(self, dname, buf_i32)
    value = buf_i32(1)
  type is (integer(int64))
    call h5read_1d(self, dname, buf_i64)
    value = buf_i64(1)
  class default
    error stop "h5fortran:read:vector_scalar: unknown memory variable type" // dname
  end select
  return
endif

call h5dopen_f(self%lid, dname, dset_id, ier)
if(ier/=0) error stop 'h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename

call get_dset_class(self, dname, dclass, dset_id)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
!! https://support.hdfgroup.org/HDF5/doc/UG/HDF5_Users_Guide-Responsive%20HTML5/index.html#t=HDF5_Users_Guide%2FDatatypes%2FHDF5_Datatypes.htm%23TOC_6_10_Data_Transferbc-26&rhtocid=6.5_2
if(dclass == H5T_FLOAT_F) then
  select type(value)
  type is (real(real64))
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, value, dims, ier)
  type is (real(real32))
    call h5dread_f(dset_id, H5T_NATIVE_REAL, value, dims, ier)
  class default
    error stop 'h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(dclass == H5T_INTEGER_F) then
  select type(value)
  type is (integer(int32))
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, value, dims, ier)
  type is (integer(int64))
    call h5dread_f(dset_id, H5T_STD_I64LE, value, dims, ier)
  class default
    error stop 'h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
elseif(dclass == H5T_STRING_F) then
  select type(value)
  type is (character(*))
    call H5Dget_type_f(dset_id, type_id, ier)
    if(ier/=0) error stop "h5fortran:read:h5tget_type " // dname // " in " // self%filename
    call H5Tget_size_f(type_id, dsize, ier)
    if(ier/=0) error stop "h5fortran:read:h5tget_size " // dname // " in " // self%filename

    if(dsize > len(value)) then
      write(stderr,'(a,i0,a3,i0,1x,a)') "h5fortran:read:string: buffer too small: ", dsize, " > ", len(value), &
          dname // " in " // self%filename
      error stop
    endif

    block
    character(dsize) :: buf_char

    call h5ltread_dataset_string_f(self%lid, dname, buf_char, ier)
    value = buf_char
    end block
  class default
    error stop "h5fortran:read: character disk dataset " // dname // " needs character memory variable"
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call h5dclose_f(dset_id, ier)
if (ier/=0) error stop 'h5fortran:reader: error closing dataset ' // dname // ' in ' // self%filename

end procedure h5read_scalar

end submodule read_scalar
