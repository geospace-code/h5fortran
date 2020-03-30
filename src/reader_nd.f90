!! conceptual--not tested--will use h5dread_f instead
submodule (h5fortran:read) reader_ND

implicit none

contains

module procedure hdf_read_8d

integer(HSIZE_T) :: dims(rank(value))
integer :: i(rank(value))
TYPE(C_PTR) :: f_ptr

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (value)
type is (real(real64))
  i = lbound(value)
  f_ptr = c_loc(value(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), f_ptr, ierr)
type is (real(real32))
  i = lbound(value)
  f_ptr = c_loc(value(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), f_ptr, ierr)
type is (integer(int32))
  i = lbound(value)
  f_ptr = c_loc(value(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), f_ptr, ierr)
type is (integer(int64))
  i = lbound(value)
  f_ptr = c_loc(value(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5ltread_dataset_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), f_ptr, ierr)
class default
  write(stderr,*) 'ERROR: ' // dname // ' datatype is not handled yet by h5fortran.'
  ierr = -1
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_8d

end submodule reader_ND