!! This submodule is for writing integer HDF5 data
submodule (hdf5_interface:write) write_int

use H5LT, only: H5_INTEGER_KIND, H5KIND_TO_TYPE
implicit none
contains


module procedure hdf_write_int

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr)
if (ierr /= 0) return

!> write dataset
call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_int


module procedure hdf_write_int_1d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr)
if (ierr /= 0) return

!> write dataset
call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_int_1d


module procedure hdf_write_int_2d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) return

!> write dataset
call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_int_2d


module procedure hdf_write_int_3d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) return

!> write dataset
call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_int_3d


module procedure hdf_write_int_4d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) return

!> write dataset
call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_int_4d


module procedure hdf_write_int_5d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) return

!> write dataset
call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_int_5d


module procedure hdf_write_int_6d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) return

!> write dataset
call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_int_6d


module procedure hdf_write_int_7d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) return

!> write dataset
call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_int_7d


end submodule write_int
