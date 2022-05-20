submodule (h5fortran) attributes

use h5lt, only: h5ltset_attribute_string_f, h5ltset_attribute_float_f, h5ltset_attribute_double_f, h5ltset_attribute_int_f, &
h5ltget_attribute_string_f, h5ltget_attribute_float_f, h5ltget_attribute_double_f, h5ltget_attribute_int_f, &
h5ltget_attribute_ndims_f, h5ltget_attribute_info_f

implicit none (type, external)

contains


module procedure readattr_char
!! NOTE: HDF5 character attributes are scalar.
integer :: ier
character(len(attrval)) :: buf

if(.not.self%is_open()) error stop 'h5fortran:readattr: file handle is not open'

call h5ltget_attribute_string_f(self%lid, dname, attr, buf, ier)
if (ier /= 0) error stop "ERROR:h5fortran:readattr_char: problem reading attr of " // dname // " in " // self%filename

attrval = buf

end procedure readattr_char


module procedure readattr_num
!! NOTE: HDF5 has 1D vector attributes for integer, float and double.
integer :: ier

if(.not.self%is_open()) error stop 'h5fortran:readattr: file handle is not open'

call attr_shape_check(self, dname, attr, size(attrval))

select type(attrval)
type is (real(real32))
  call h5ltget_attribute_float_f(self%lid, dname, attr, attrval, ier)
type is (real(real64))
  call h5ltget_attribute_double_f(self%lid, dname, attr, attrval, ier)
type is (integer(int32))
  call h5ltget_attribute_int_f(self%lid, dname, attr, attrval, ier)
class default
  error stop "ERROR:h5fortran:readattr_num: unknown dataset type for " // dname // " in " // self%filename
end select

if (ier /= 0) error stop "ERROR:h5fortran:readattr_num: " // dname // " in " // self%filename

end procedure readattr_num


module procedure writeattr_char
!! NOTE: HDF5 character attributes are scalar.

integer :: ier

if(.not.self%is_open()) error stop 'ERROR:h5fortran:writeattr: file handle is not open'

call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ier)

if (ier /= 0) error stop "ERROR:h5fortran:writeattr_char: " // dname // " in " // self%filename

end procedure writeattr_char


module procedure writeattr_num
!! NOTE: HDF5 has 1D vector attributes for integer, float and double.
integer :: ier
integer(size_t) :: dsize

if(.not.self%is_open()) error stop 'ERROR:h5fortran:writeattr: file handle is not open'

dsize = size(attrval)

select type(attrval)
type is (real(real32))
  call h5ltset_attribute_float_f(self%lid, dname, attr, attrval, dsize, ier)
type is (real(real64))
  call h5ltset_attribute_double_f(self%lid, dname, attr, attrval, dsize, ier)
type is (integer(int32))
  call h5ltset_attribute_int_f(self%lid, dname, attr, attrval, dsize, ier)
class default
  error stop "ERROR:h5fortran:writeattr_num: unknown dataset type for " // dname // " in " // self%filename
end select

if (ier /= 0) error stop "ERROR:h5fortran:writeattr_num: " // dname // " in " // self%filename

end procedure writeattr_num


module procedure writeattr_char_lt

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr_char(dname, attr, attrval)
call h%close()

end procedure writeattr_char_lt


module procedure writeattr_num_lt

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr_num(dname, attr, attrval)
call h%close()

end procedure writeattr_num_lt


module procedure readattr_char_lt

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr_char(dname, attr, attrval)
call h%close()

end procedure readattr_char_lt


module procedure readattr_num_lt

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr_num(dname, attr, attrval)
call h%close()

end procedure readattr_num_lt


subroutine attr_shape_check(self, dname, attr, asize)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
integer, intent(in) :: asize

integer :: arank, atype, ierr
integer(size_t) :: attr_bytes
integer(hsize_t) :: adims(1)

if(.not. self%is_open()) error stop 'h5fortran:attr_shape: file handle is not open'

if (.not. self%exist(dname)) then
  error stop 'ERROR:h5fortran ' // dname // ' attribute ' // attr // ' does not exist in ' // self%filename
endif

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_attribute_ndims_f(self%lid, dname, attr, arank, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:get_attribute_ndims: ' // dname // ' ' // self%filename

if (arank /= 1) then
  write(stderr,'(A,I6,A,I6)') 'ERROR: attribute rank mismatch ' // dname // ' attribute "' // attr // '" = ', &
    arank,'  should be 1'
  error stop
endif

!> check for matching size, else bad reads can occur.

call h5ltget_attribute_info_f(self%lid, dname, attr, adims, atype, attr_bytes, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran: get_attribute_info' // dname // ' read ' // self%filename

if(.not. all(asize == adims)) then
  write(stderr,*) 'ERROR:h5fortran: shape mismatch ' // dname // ' attribute "' // attr //'" = ', adims,'  shape =', asize
  error stop
endif

end subroutine attr_shape_check


end submodule attributes
