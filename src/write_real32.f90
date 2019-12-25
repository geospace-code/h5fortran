!! This submodule is for writing single precision float (32 bit)
submodule (hdf5_interface:write) write_real32

use H5LT, only: H5_REAL_KIND, H5S_SCALAR_F, H5KIND_TO_TYPE
implicit none
contains


module procedure hdf_write_real32

integer(HID_T) :: sid,did

call self%write(dname, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' ' // self%filename
  return
endif

!> HDF5 >= 1.10
!call h5ltmake_dataset_f(self%lid, dname, &
!   rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_REAL_KIND), value, ierr)
!if (ierr /= 0) then
!   write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
!   return
!endif

!> HDF5 1.8 compatbility below:
!> create dataspace
call h5screate_f(H5S_SCALAR_F, sid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create dataspace ' // dname // ' write ' // self%filename
  return
endif

!> create dataset
call h5dcreate_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), sid, did, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

!> write dataset
call h5dwrite_f(did, h5kind_to_type(kind(value),H5_REAL_KIND), value, int(shape(value),HSIZE_T), ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

!> close space and dataset
call h5dclose_f(did, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: close ' // dname // ' write ' // self%filename
  return
endif

call h5sclose_f(sid, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: close ' // dname // ' write ' // self%filename
  return
endif

end procedure hdf_write_real32


module procedure hdf_write_real32_1d

call self%write(dname, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' ' // self%filename
  return
endif

call h5ltmake_dataset_f(self%lid, dname, &
  rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_REAL_KIND), value, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

end procedure hdf_write_real32_1d


module procedure hdf_write_real32_2d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' close ' // self%filename
  return
endif

end procedure hdf_write_real32_2d


module procedure hdf_write_real32_3d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' close ' // self%filename
  return
endif

end procedure hdf_write_real32_3d


module procedure hdf_write_real32_4d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' close ' // self%filename
  return
endif

end procedure hdf_write_real32_4d


module procedure hdf_write_real32_5d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' close ' // self%filename
  return
endif

end procedure hdf_write_real32_5d


module procedure hdf_write_real32_6d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' close ' // self%filename
  return
endif

end procedure hdf_write_real32_6d


module procedure hdf_write_real32_7d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, ierr, chunk_size)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' write ' // self%filename
  return
endif

call hdf_wrapup(self, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' close ' // self%filename
  return
endif

end procedure hdf_write_real32_7d


end submodule write_real32
