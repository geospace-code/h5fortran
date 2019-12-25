!! This submodule is for writing integer HDF5 data
submodule (hdf5_interface:write) write_int

use H5LT, only: h5dopen_f, H5S_SCALAR_F, H5_INTEGER_KIND, H5KIND_TO_TYPE
implicit none
contains


module procedure hdf_write_int

integer(HID_T) :: sid,did
logical :: exists

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: ' // dname // ' check exist ' // self%filename
  return
endif


if(exists) then
  !> open dataset
  call h5dopen_f(self%lid, dname, did, ierr)
  if (ierr /= 0) then
    write(stderr,*) 'ERROR: open ' // dname // ' ' // self%filename
    return
  endif
else
  call self%write(dname, ierr)
  if (ierr /= 0) then
    write(stderr,*) 'ERROR: create ' // dname // ' ' // self%filename
    return
  endif

  !> create dataspace
  call h5screate_f(H5S_SCALAR_F, sid, ierr)
  if (ierr /= 0) then
    write(stderr,*) 'ERROR: create dataspace ' // dname // ' write ' // self%filename
    return
  endif

  !> create dataset
  call h5dcreate_f(self%lid, dname, h5kind_to_type(kind(value),H5_INTEGER_KIND), sid, did, ierr)
  if (ierr /= 0) then
    write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
    return
  endif
endif

!> write dataset
call h5dwrite_f(did, h5kind_to_type(kind(value),H5_INTEGER_KIND), value, int(shape(value),HSIZE_T), ierr)
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

if(exists) then
  call h5sclose_f(sid, ierr)
  if (ierr /= 0) then
    write(stderr,*) 'ERROR: close ' // dname // ' write ' // self%filename
    return
  endif
endif

end procedure hdf_write_int


module procedure hdf_write_int_1d

call self%write(dname, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' ' // self%filename
  return
endif

call h5ltmake_dataset_f(self%lid, dname, &
  rank(value), int(shape(value),HSIZE_T), h5kind_to_type(kind(value),H5_INTEGER_KIND), value, ierr)
if (ierr /= 0) then
  write(stderr,*) 'ERROR: create ' // dname // ' write ' // self%filename
  return
endif

end procedure hdf_write_int_1d


module procedure hdf_write_int_2d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

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

end procedure hdf_write_int_2d


module procedure hdf_write_int_3d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

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

end procedure hdf_write_int_3d


module procedure hdf_write_int_4d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

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

end procedure hdf_write_int_4d


module procedure hdf_write_int_5d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

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

end procedure hdf_write_int_5d


module procedure hdf_write_int_6d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))

dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

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

end procedure hdf_write_int_6d


module procedure hdf_write_int_7d

integer(HID_T)  :: dtype
integer(HSIZE_T) :: dims(rank(value))


dims = shape(value)
dtype = h5kind_to_type(kind(value),H5_INTEGER_KIND)

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

end procedure hdf_write_int_7d


end submodule write_int
