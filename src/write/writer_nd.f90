!! conceptual--not tested
submodule (h5fortran:write) writer_ND

implicit none (type, external)

contains

module procedure h5write_8d

integer(HID_T)  :: dtype, filespace_id, dset_id
integer(HSIZE_T) :: dims(rank(value))
integer :: i(rank(value))
TYPE(C_PTR) :: f_ptr

ierr = 0

select type (value)
type is (real(real64))
  dims = shape(value, HSIZE_T)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  call hdf_create(self,dname,dtype,dims,filespace_id,dset_id)

  i = lbound(value)
  f_ptr = c_loc(value(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5dwrite_f(dset_id, dtype, f_ptr, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value, HSIZE_T)
  call hdf_create(self,dname,dtype,dims,filespace_id,dset_id)

  i = lbound(value)
  f_ptr = c_loc(value(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5dwrite_f(dset_id, dtype, f_ptr, ierr)
type is (integer(int32))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value, HSIZE_T)
  call hdf_create(self,dname,dtype,dims,filespace_id,dset_id)

  i = lbound(value)
  f_ptr = c_loc(value(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5dwrite_f(dset_id, dtype, f_ptr, ierr)
type is (integer(int64))
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value, HSIZE_T)
  call hdf_create(self,dname,dtype,dims,filespace_id,dset_id)

  i = lbound(value)
  f_ptr = c_loc(value(i(1),i(2),i(3),i(4),i(5),i(6),i(7),i(8)))
  call h5dwrite_f(dset_id, dtype, f_ptr, ierr)
class default
  ierr = 6
end select

call hdf_wrapup(dset_id, filespace_id)
if (check(ierr, 'ERROR:h5fortran: ' // dname // ' write ' // self%filename))  return

end procedure h5write_8d

end submodule writer_ND
