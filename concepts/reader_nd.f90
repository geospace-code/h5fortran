!! conceptual--not tested--will use h5dread_f instead
submodule (h5fortran:read) reader_ND

implicit none (type, external)

contains

module procedure hdf_read_8d

integer(HSIZE_T) :: dims(rank(A))
integer :: i(rank(A))
TYPE(C_PTR) :: f_ptr

call hdf_setup_read(self, dname, dims, ierr)
if (ierr /= 0) return

select type (A)
type is (real(real64))
  i = lbound(A)
  f_ptr = c_loc(A(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5ltread_dataset_f(self%file_id, dname, h5kind_to_type(kind(A),H5_REAL_KIND), f_ptr, ierr)
type is (real(real32))
  i = lbound(A)
  f_ptr = c_loc(A(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5ltread_dataset_f(self%file_id, dname, h5kind_to_type(kind(A),H5_REAL_KIND), f_ptr, ierr)
type is (integer(int32))
  i = lbound(A)
  f_ptr = c_loc(A(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5ltread_dataset_f(self%file_id, dname, h5kind_to_type(kind(A),H5_INTEGER_KIND), f_ptr, ierr)
type is (integer(int64))
  i = lbound(A)
  f_ptr = c_loc(A(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5ltread_dataset_f(self%file_id, dname, h5kind_to_type(kind(A),H5_INTEGER_KIND), f_ptr, ierr)
class default
  ierr = 6
end select

if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' read ' // self%filename

end procedure hdf_read_8d

end submodule reader_ND
