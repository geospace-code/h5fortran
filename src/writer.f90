!! This submodule is for writing double precision float (64 bit)
submodule (hdf5_interface:write) writer_ND

use H5LT, only: H5_REAL_KIND, H5_INTEGER_KIND, H5S_SCALAR_F, H5KIND_TO_TYPE
implicit none
contains


module procedure hdf_write_scalar

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (character(*))
  call h5ltmake_dataset_string_f(self%lid, dname, value, ierr)
  if (ierr /= 0) write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
type is (real(real64))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (integer)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
end select

if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_scalar


module procedure hdf_write_1d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (integer)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
end select

if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_1d


module procedure hdf_write_2d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (integer)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
end select

if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_2d


module procedure hdf_write_3d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (integer)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
end select

if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_3d


module procedure hdf_write_4d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (integer)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
end select

if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_4d


module procedure hdf_write_5d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (integer)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
end select

if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_5d


module procedure hdf_write_6d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (integer)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
end select

if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_6d


module procedure hdf_write_7d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

select type (value)
type is (real(real64))
  dims = shape(value)
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (real(real32))
  dtype = h5kind_to_type(kind(value),H5_REAL_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
type is (integer)
  dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)
  dims = shape(value)

  call hdf_setup_write(self,dname,dtype,dims, ierr)
  if (ierr /= 0) return

  call h5dwrite_f(self%did, dtype, value, dims, ierr)
end select

if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) return

end procedure hdf_write_7d


end submodule writer_ND
