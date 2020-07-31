submodule (h5fortran) attributes

use h5lt, only: h5ltset_attribute_string_f, h5ltset_attribute_float_f, h5ltset_attribute_double_f, h5ltset_attribute_int_f

implicit none (type, external)

contains


module procedure writeattr_char
!! NOTE: HDF5 character attributes are scalar.

integer :: ier

if(.not.self%is_open) error stop 'h5fortran:writeattr: file handle is not open'

call h5ltset_attribute_string_f(self%lid, dname, attr, attrval, ier)

if (present(ierr)) ierr = ier
if (check(ier, self%filename, dname)) then
  if (present(ierr)) return
  error stop
endif

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
if (check(ier, self%filename, dname)) then
  if (present(ierr)) return
  error stop
endif

end procedure writeattr_num


end submodule attributes
