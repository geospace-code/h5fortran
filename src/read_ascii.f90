submodule (h5fortran:read_scalar) read_scalar_ascii

use, intrinsic :: iso_c_binding, only : C_CHAR, C_F_POINTER
use hdf5, only : H5T_STR_NULLTERM_F, &
H5Dvlen_get_max_len_f, H5Dread_vl_f, H5Dvlen_reclaim_f, &
H5Tis_variable_str_f

implicit none (type, external)

contains

module procedure read_scalar_char

integer(HID_T) :: type_id
integer :: ier, i, pad_type, L
integer(SIZE_T) :: dsize
logical :: vstatus

!> variable length string
integer(HSIZE_T) :: dset_dims(1), maxdims(1), dims(0)
TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
TYPE(C_PTR) :: f_ptr

CHARACTER(10000, kind=c_char), POINTER :: cstr !< arbitrary maximum variable length string
character(:), allocatable :: buf_char !< fixed length read
character(1000) :: dset_name !< no specific maximum length for dataset name

dset_name = id2name(dset_id)
dset_dims = [1]

select type(A)
type is (character(*)) !< kind=c_char too
  L = len(A)
class default
  error stop 'ERROR:h5fortran:read: character disk dataset ' // trim(dset_name) // ' needs character memory variable'
end select

call H5Dget_type_f(dset_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tget_type " // trim(dset_name)

call H5Tis_variable_str_f(type_id, vstatus, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tis_variable_str " // trim(dset_name)

if(vstatus) then
  !call H5Dvlen_get_max_len_f(dset_id, type_id, space_id, dsize, ier)
  !if(ier/=0) error stop "h5fortran:read:H5Dvlen_get_max_len " // trim(dset_name)

  call H5Sget_simple_extent_dims_f(mem_space_id, dset_dims, maxdims, ier)
  if(ier/=0) error stop "h5fortran:read:vlen_char:H5Sget_simple_extent_dim " // trim(dset_name)

  allocate(cbuf(1:dset_dims(1)))
  f_ptr = C_LOC(cbuf(1))

  call H5Dread_f(dset_id, type_id, f_ptr, ier, mem_space_id, file_space_id)
  if(ier/=0) error stop "h5fortran:read:vlen_char:H5Dread " // trim(dset_name)

  call C_F_POINTER(cbuf(1), cstr)

  i = index(cstr, c_null_char) - 1
  if (i == -1) i = len_trim(cstr)
  if(i > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:read:vlen_char: buffer too small: ", i, " > ", L, trim(dset_name)
    error stop
  endif

  select type(A)
  type is (character(*)) !< kind=c_char too
    A = cstr(:i)
  end select
else
  call H5Tget_strpad_f(type_id, pad_type, ier)
  if(ier/=0) error stop "ERROR:h5fortran:read:H5Tget_strpad " // trim(dset_name)

  call H5Tget_size_f(type_id, dsize, ier) !< only for non-variable
  if(ier/=0) error stop "ERROR:h5fortran:read:H5Tget_size " // trim(dset_name)

  if(dsize > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:read:string: buffer too small: ", dsize, " > ", L, trim(dset_name)
    error stop
  endif

  allocate(character(dsize) :: buf_char)

  call H5Dread_f(dset_id, type_id, buf_char, dims, ier, mem_space_id, file_space_id)
  if(ier/=0) error stop "ERROR:h5fortran:read:H5Dread character " // trim(dset_name)

  i = index(buf_char, c_null_char) - 1
  if (i == -1) i = len_trim(buf_char)

  select type(A)
  type is (character(*)) !< kind=c_char too
    A = buf_char(:i)
  end select
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // trim(dset_name)

end procedure read_scalar_char

end submodule read_scalar_ascii
