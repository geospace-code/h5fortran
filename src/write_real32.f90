!! This submodule is for writing single precision float (32 bit)
submodule (hdf5_interface:write) write_real32

use H5LT, only: H5_REAL_KIND, H5S_SCALAR_F, H5KIND_TO_TYPE
implicit none
contains


module procedure hdf_add_real32

integer(HID_T) :: sid,did
integer         :: ierr

call self%add(dname)

!> HDF5 >= 1.10
!call h5ltmake_dataset_f(self%lid, dname, &
!   rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_REAL_KIND), value, ierr)
!if (ierr /= 0) error stop 'dataset ' // dname // ' write ' // self%filename

!> HDF5 1.8 compatbility below:
!> create dataspace
call h5screate_f(H5S_SCALAR_F, sid, ierr)
if (ierr /= 0) error stop 'create dataspace ' // dname // ' write ' // self%filename

!> create dataset
call h5dcreate_f(self%lid, dname, h5kind_to_type(kind(value),H5_REAL_KIND), sid, did, ierr)
if (ierr /= 0) error stop 'create dataset ' // dname // ' write ' // self%filename

!> write dataset
call h5dwrite_f(did, h5kind_to_type(kind(value),H5_REAL_KIND), value, int(shape(value),HSIZE_T), ierr)
if (ierr /= 0) error stop 'write dataset ' // dname // ' write ' // self%filename

!> close space and dataset
call h5dclose_f(did, ierr)
if (ierr /= 0) error stop 'close dataset ' // dname // ' write ' // self%filename
call h5sclose_f(sid, ierr)
if (ierr /= 0) error stop 'close dataspace ' // dname // ' write ' // self%filename

end procedure hdf_add_real32


module procedure hdf_add_real32_1d

integer         :: ierr

call self%add(dname)

call h5ltmake_dataset_f(self%lid, dname, &
  rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_REAL_KIND), value, ierr)
if (ierr /= 0) error stop 'create dataset ' // dname // ' write ' // self%filename

end procedure hdf_add_real32_1d


module procedure hdf_add_real32_2d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'dataset ' // dname // ' write ' // self%filename

call hdf_wrapup(self)

end procedure hdf_add_real32_2d


module procedure hdf_add_real32_3d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'dataset ' // dname // ' write ' // self%filename

call hdf_wrapup(self)

end procedure hdf_add_real32_3d


module procedure hdf_add_real32_4d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'dataset ' // dname // ' write ' // self%filename

call hdf_wrapup(self)

end procedure hdf_add_real32_4d


module procedure hdf_add_real32_5d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'dataset ' // dname // ' write ' // self%filename

call hdf_wrapup(self)

end procedure hdf_add_real32_5d


module procedure hdf_add_real32_6d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'dataset ' // dname // ' write ' // self%filename

call hdf_wrapup(self)

end procedure hdf_add_real32_6d


module procedure hdf_add_real32_7d

integer         :: ierr
integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_REAL_KIND)

call hdf_setup_write(self,dname,dtype,dims, chunk_size)

call h5dwrite_f(self%did, dtype, value, dims, ierr)
if (ierr /= 0) error stop 'dataset ' //dname// ' write ' // self%filename

call hdf_wrapup(self)

end procedure hdf_add_real32_7d


end submodule write_real32
