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

if(.not.self%is_open) error stop 'h5fortran:readattr: file handle is not open'

call h5ltget_attribute_string_f(self%lid, dname, attr, buf, ier)
attrval = buf

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not. present(ierr)) error stop

end procedure readattr_char


module procedure readattr_num
!! NOTE: HDF5 has 1D vector attributes for integer, float and double.
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:readattr: file handle is not open'

call attr_shape_check(self, dname, attr, size(attrval), ier)

if(ier==0) then
select type(attrval)
type is (real(real32))
  call h5ltget_attribute_float_f(self%lid, dname, attr, attrval, ier)
type is (real(real64))
  call h5ltget_attribute_double_f(self%lid, dname, attr, attrval, ier)
type is (integer(int32))
  call h5ltget_attribute_int_f(self%lid, dname, attr, attrval, ier)
class default
  ier = 6
end select
endif

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not. present(ierr)) error stop

end procedure readattr_num


module procedure writeattr_char
!! NOTE: HDF5 character attributes are scalar.

integer :: ier

if(.not.self%is_open) error stop 'h5fortran:writeattr: file handle is not open'

call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not. present(ierr)) error stop

end procedure writeattr_char


module procedure writeattr_num
!! NOTE: HDF5 has 1D vector attributes for integer, float and double.
integer :: ier
integer(size_t) :: dsize

if(.not.self%is_open) error stop 'h5fortran:writeattr: file handle is not open'

dsize = size(attrval)

select type(attrval)
type is (real(real32))
  call h5ltset_attribute_float_f(self%lid, dname, attr, attrval, dsize, ier)
type is (real(real64))
  call h5ltset_attribute_double_f(self%lid, dname, attr, attrval, dsize, ier)
type is (integer(int32))
  call h5ltset_attribute_int_f(self%lid, dname, attr, attrval, dsize, ier)
class default
  ier = 6
end select

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname) .and. .not. present(ierr)) error stop

end procedure writeattr_num


module procedure writeattr_char_lt

type(hdf5_file) :: h
integer :: ier

call h%open(filename, ier, action='r+')

call h%writeattr_char(dname, attr, attrval, ier)

if (ier == 0) call h%close(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname) .and. .not. present(ierr)) error stop

end procedure writeattr_char_lt


module procedure writeattr_num_lt

type(hdf5_file) :: h
integer :: ier

call h%open(filename, ier, action='r+')

call h%writeattr_num(dname, attr, attrval, ier)

if (ier == 0) call h%close(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname) .and. .not. present(ierr)) error stop

end procedure writeattr_num_lt


module procedure readattr_char_lt

type(hdf5_file) :: h
integer :: ier

call h%open(filename, ier, action='r')

call h%readattr_char(dname, attr, attrval, ier)

if (ier == 0) call h%close(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname) .and. .not. present(ierr)) error stop

end procedure readattr_char_lt


module procedure readattr_num_lt

type(hdf5_file) :: h
integer :: ier

call h%open(filename, ier, action='r')

call h%readattr_num(dname, attr, attrval, ier)

if (ier == 0) call h%close(ier)

if (present(ierr)) ierr = ier
if (check(ier, filename, dname) .and. .not. present(ierr)) error stop

end procedure readattr_num_lt


subroutine attr_shape_check(self, dname, attr, asize, ierr)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
integer, intent(in) :: asize
integer, intent(out) :: ierr

integer :: arank, atype
integer(size_t) :: attr_bytes
integer(hsize_t) :: adims(1)

if(.not.self%is_open) error stop 'h5fortran:attr_shape: file handle is not open'

if (.not.self%exist(dname)) then
  write(stderr,*) 'ERROR: ' // dname // ' attribute ' // attr // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_attribute_ndims_f(self%lid, dname, attr, arank, ierr)
if (check(ierr, 'ERROR:get_attribute_ndims: ' // dname // ' ' // self%filename)) return

if (arank /= 1) then
  write(stderr,'(A,I6,A,I6)') 'ERROR: attribute rank mismatch ' // dname // ' attribute "' // attr // '" = ', &
    arank,'  should be 1'
  ierr = -1
  return
endif

!> check for matching size, else bad reads can occur.

call h5ltget_attribute_info_f(self%lid, dname, attr, adims, atype, attr_bytes, ierr)
if (check(ierr, 'ERROR: get_attribute_info' // dname // ' read ' // self%filename)) return

if(.not. all(asize == adims)) then
  write(stderr,*) 'ERROR: shape mismatch ' // dname // ' attribute "' // attr //'" = ', adims,'  shape =', asize
  ierr = -1
  return
endif

end subroutine attr_shape_check


end submodule attributes
