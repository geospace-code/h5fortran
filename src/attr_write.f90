submodule (h5fortran) attr_write

use h5lt, only: h5ltset_attribute_string_f, h5ltset_attribute_float_f, h5ltset_attribute_double_f, h5ltset_attribute_int_f

implicit none (type, external)

contains

module procedure writeattr_char
!! NOTE: HDF5 character attributes are scalar.

integer :: ier

if(.not.self%is_open()) error stop 'ERROR:h5fortran:writeattr: file handle is not open'

call h5ltset_attribute_string_f(self%file_id, dname, attr, A, ier)

if (ier /= 0) error stop "ERROR:h5fortran:writeattr_char: " // dname // " in " // self%filename

end procedure writeattr_char


module procedure writeattr_num
!! NOTE: HDF5 has 1D vector attributes for integer, float and double.
integer :: ier
integer(size_t) :: dsize

if(.not.self%is_open()) error stop 'ERROR:h5fortran:writeattr: file handle is not open'

dsize = size(A)

select type(A)
type is (real(real32))
  call h5ltset_attribute_float_f(self%file_id, dname, attr, A, dsize, ier)
type is (real(real64))
  call h5ltset_attribute_double_f(self%file_id, dname, attr, A, dsize, ier)
type is (integer(int32))
  call h5ltset_attribute_int_f(self%file_id, dname, attr, A, dsize, ier)
class default
  error stop "ERROR:h5fortran:writeattr_num: unknown dataset type for " // dname // " in " // self%filename
end select

if (ier /= 0) error stop "ERROR:h5fortran:writeattr_num: " // dname // " in " // self%filename

end procedure writeattr_num


module procedure writeattr_char_lt

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr_char(dname, attr, A)
call h%close()

end procedure writeattr_char_lt


module procedure writeattr_num_lt

type(hdf5_file) :: h

call h%open(filename, action='r+')
call h%writeattr_num(dname, attr, A)
call h%close()

end procedure writeattr_num_lt

end submodule attr_write
