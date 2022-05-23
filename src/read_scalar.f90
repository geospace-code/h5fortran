submodule (h5fortran:hdf5_read) read_scalar

use h5lt, only : h5ltread_dataset_string_f
use hdf5, only : h5dread_f, h5dget_space_f, h5dvlen_get_max_len_f, h5dread_vl_f, h5dvlen_reclaim_f,&
h5tis_variable_str_f, &
h5sclose_f, &
H5T_STR_NULLTERM_F

implicit none (type, external)

contains


module procedure h5read_scalar

integer(HSIZE_T) :: dims(0)
integer(SIZE_T) :: dsize
integer(HID_T) :: dset_id, type_id, space_id
integer :: dclass, ier, i, pad_type

logical :: vector_scalar, vstatus

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
    call h5tis_variable_str_f(type_id, vstatus, ier)
    if(ier/=0) error stop "h5fortran:read:h5tis_variable_str " // dname // " in " // self%filename

    if(vstatus) then
      call H5Dget_space_f(dset_id, space_id, ier)
      if(ier/=0) error stop "h5fortran:read:h5dget_space " // dname // " in " // self%filename
      !call h5dvlen_get_max_len_f(dset_id, type_id, space_id, dsize, ier)
      !if(ier/=0) error stop "h5fortran:read:h5dvlen_get_max_len " // dname // " in " // self%filename

      block
      character(10000) :: buf_char(1)
      !! TODO: dynamically determine buffer size
      integer(HSIZE_T) :: vldims(2)
      integer(SIZE_T) :: vlen(1)

      vldims = [len(buf_char), 1]

      call h5dread_vl_f(dset_id, type_id, buf_char, vldims, vlen, hdferr=ier, mem_space_id=space_id)
      if(ier/=0) error stop "h5fortran:read:h5dread_vl " // dname // " in " // self%filename

      i = index(buf_char(1), c_null_char) - 1
      if (i == -1) i = len_trim(buf_char(1))

      value = buf_char(1)(:i)

      ! call h5dvlen_reclaim_f(type_id, H5S_ALL_F, H5P_DEFAULT_F, buf_char, ier)
      end block

      call h5sclose_f(space_id, ier)
      if(ier/=0) error stop "h5fortran:read:h5sclose " // dname // " in " // self%filename
    else
      call H5Tget_strpad_f(type_id, pad_type, ier)
      if(ier/=0) error stop "h5fortran:read:h5tget_strpad " // dname // " in " // self%filename

      call H5Tget_size_f(type_id, dsize, ier) !< only for non-variable
      if(ier/=0) error stop "h5fortran:read:h5tget_size " // dname // " in " // self%filename

      if(dsize > len(value)) then
        write(stderr,'(a,i0,a3,i0,1x,a)') "h5fortran:read:string: buffer too small: ", dsize, " > ", len(value), &
            dname // " in " // self%filename
        error stop
      endif

      block
      character(dsize) :: buf_char

      call h5ltread_dataset_string_f(self%lid, dname, buf_char, ier)
      if(ier/=0) error stop "h5fortran:read:h5l5read_dataset_string " // dname // " in " // self%filename

      i = index(buf_char, c_null_char) - 1
      if (i == -1) i = len_trim(buf_char)

      value = buf_char(:i)
      end block
    endif

    call h5tclose_f(type_id, ier)
    if(ier/=0) error stop "h5fortran:read:h5tclose " // dname // " in " // self%filename

  class default
    error stop "h5fortran:read: character disk dataset " // dname // " needs character memory variable"
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader: closing dataset: " // dname // " in " // self%filename


end procedure h5read_scalar

end submodule read_scalar
