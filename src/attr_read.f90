submodule (h5fortran) attr_read

use, intrinsic :: iso_c_binding, only : C_CHAR, C_NULL_CHAR, C_F_POINTER

use hdf5, only : H5Aexists_by_name_f, H5Aopen_by_name_f, H5Aread_f, H5Aclose_f, H5Aget_info_f, H5Aget_type_f, &
H5Tclose_f, H5Tis_variable_str_f
use h5lt, only: h5ltget_attribute_float_f, h5ltget_attribute_double_f, h5ltget_attribute_int_f, &
h5ltget_attribute_ndims_f, h5ltget_attribute_info_f

implicit none (type, external)

contains


module procedure readattr_char
!! NOTE: HDF5 character attributes are scalar.
integer(HID_T) :: type_id, attr_id
integer :: ier, i, L
integer(HSIZE_T) :: dsize

logical :: vstatus, f_corder_valid
integer :: corder, cset
logical :: attr_exists

!> variable length string
TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
TYPE(C_PTR) :: f_ptr

CHARACTER(10000, kind=c_char), POINTER :: cstr !< arbitrary maximum variable length string
character(:), allocatable :: buf_char !< fixed length read

integer(HSIZE_T) :: dims(0)

L = len(A)

call h5aexists_by_name_f(self%file_id, dname, attr, attr_exists, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:h5aexists_by_name_f failed: " // dname // " attr: " // attr
if(.not.attr_exists) error stop 'h5fortran:readattr: attribute not exist: ' // dname // " attr: " // attr

call H5Aopen_by_name_f(self%file_id, dname, attr, attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name_f failed: " // dname // " attr: " // attr

call H5Aget_type_f(attr_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_type " // dname
call H5Aget_info_f(attr_id, f_corder_valid, corder, cset, dsize, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_info " // dname

call H5Tis_variable_str_f(type_id, vstatus, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Tis_variable_str " // trim(dname)

if(vstatus) then
  ! call H5Aget_space_f(attr_id, space_id, ier)
  ! if(ier/=0) error stop "ERROR:h5fortran:read:readattr:H5Aget_space " // dname // " attr: " // attr

  allocate(cbuf(1:dsize))
  f_ptr = C_LOC(cbuf(1))

  call H5Aread_f(attr_id, type_id, f_ptr, ier)
  if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // dname // " attr: " // attr

  call C_F_POINTER(cbuf(1), cstr)

  i = index(cstr, c_null_char) - 1
  if (i == -1) i = len_trim(cstr)
  if(i > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr_vlen: buffer too small: ", i, " > ", L, dname // ":" // attr
    error stop
  endif

  A = cstr(:i)
else
  if(dsize > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", dsize, " > ", L, dname // ":" // attr
    error stop
  endif

  allocate(character(dsize) :: buf_char)

  call H5Aread_f(attr_id, type_id, buf_char, dims, ier)
  if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // dname // " attr: " // attr

  i = index(buf_char, c_null_char) - 1
  if (i == -1) i = len_trim(buf_char)

  A = buf_char(:i)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // dname // " attr: " // attr

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aclose " // dname // " attr: " // attr

end procedure readattr_char


module procedure readattr_num
!! NOTE: HDF5 has 1D vector attributes for integer, float and double.
integer :: ier

call attr_shape_check(self, dname, attr, size(A))

select type(A)
type is (real(real32))
  call h5ltget_attribute_float_f(self%file_id, dname, attr, A, ier)
type is (real(real64))
  call h5ltget_attribute_double_f(self%file_id, dname, attr, A, ier)
type is (integer(int32))
  call h5ltget_attribute_int_f(self%file_id, dname, attr, A, ier)
class default
  error stop "ERROR:h5fortran:readattr_num: unknown dataset type for " // dname // " in " // self%filename
end select

if (ier /= 0) error stop "ERROR:h5fortran:readattr_num: " // dname // " in " // self%filename

end procedure readattr_num


module procedure readattr_char_lt

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr_char(dname, attr, A)
call h%close()

end procedure readattr_char_lt


module procedure readattr_num_lt

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr_num(dname, attr, A)
call h%close()

end procedure readattr_num_lt


subroutine attr_shape_check(self, dname, attr, asize)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
integer, intent(in) :: asize

integer :: arank, atype, ierr
integer(size_t) :: attr_bytes
integer(hsize_t) :: adims(1)

if (.not. self%exist(dname)) error stop 'ERROR:h5fortran ' // dname // ' attr: ' // attr // ' does not exist in ' // self%filename

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_attribute_ndims_f(self%file_id, dname, attr, arank, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:get_attribute_ndims: ' // dname // ' ' // self%filename

if (arank /= 1) then
  write(stderr,'(A,I6,A,I6)') 'ERROR:h5fortran: attribute rank mismatch ' // dname // ' attr: "' // attr // '" = ', arank,' /= 1'
  error stop
endif

!> check for matching size, else bad reads can occur.

call h5ltget_attribute_info_f(self%file_id, dname, attr, adims, atype, attr_bytes, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran: get_attribute_info' // dname // ' read ' // self%filename

if(.not. all(asize == adims)) then
  write(stderr,*) 'ERROR:h5fortran: shape mismatch ' // dname // ' attribute "' // attr //'" = ', adims,'  shape =', asize
  error stop
endif

end subroutine attr_shape_check


end submodule attr_read
