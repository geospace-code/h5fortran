submodule (h5fortran) read
!! This submodule is for reading HDF5 via submodules
use hdf5, only : h5dget_create_plist_f, &
  h5pget_layout_f, h5pget_chunk_f, &
  h5dget_type_f, h5tget_native_type_f, h5tget_class_f, H5Tget_order_f, h5tclose_f, h5tget_size_f, &
  H5T_DIR_ASCEND_F, H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F, H5T_STD_I64LE

use H5LT, only : h5ltpath_valid_f

implicit none (type, external)

contains


integer(hid_t) function get_native_dtype(ds_id, dname, filename) result(native_dtype)

integer(hid_t), intent(in) :: ds_id
character(*), intent(in) :: dname, filename

integer(hid_t) :: dtype_id, native_dtype_id
integer :: class
integer :: ierr
! integer :: order, machine_order
integer(size_t) :: size_bytes

!> get the dataset variable type
!! the "type" and "native_type" are just IDs, the final native type is composed from:
!! * enddianness
!! * generic type
call h5dget_type_f(ds_id, dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:reader: get internal dtype ' // dname // ' from ' // filename

call h5tget_native_type_f(dtype_id, H5T_DIR_ASCEND_F, native_dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:reader: get native dtype id ' // dname // ' from ' // filename

!> we think endianness is handled by HDF5 ... ?
! call h5tget_order_f(native_dtype_id, order, ierr)
! if(ierr/=0) error stop 'h5fortran:reader: get endianness ' // dname // ' from ' // filename
! !> check dataset endianness matches machine (in future, could swap endianness if needed)
! call h5tget_order_f(H5T_NATIVE_INTEGER, machine_order, ierr)
! if(order /= machine_order) error stop 'h5fortran:reader: endianness does not match machine native ' // dname // ' from ' // filename

!> compose datatype inferred
call h5tget_class_f(native_dtype_id, class, ierr)
if(ierr/=0) error stop 'h5fortran:reader: get class ' // dname // ' from ' // filename

call h5tget_size_f(native_dtype_id, size_bytes, ierr)
if(ierr/=0) error stop 'h5fortran:reader: get byte size ' // dname // ' from ' // filename

call h5tclose_f(dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:reader: closing dtype ' // dname // ' from ' // filename


if(class == H5T_INTEGER_F) then
  if(size_bytes == 4) then
    native_dtype = H5T_NATIVE_INTEGER
  elseif(size_bytes == 8) then
    native_dtype = H5T_STD_I64LE
  else
    error stop "h5fortran:reader: expected 32-bit or 64-bit integer:" // dname // ' from ' // filename
  endif
elseif(class == H5T_FLOAT_F) then
  if(size_bytes == 4) then
    native_dtype = H5T_NATIVE_REAL
  elseif(size_bytes == 8) then
    native_dtype = H5T_NATIVE_DOUBLE
  else
    error stop "h5fortran:reader: expected 32-bit or 64-bit real:" // dname // ' from ' // filename
  endif
elseif(class == H5T_STRING_F) then
  native_dtype = H5T_NATIVE_CHARACTER
else
  error stop "h5fortran:reader: non-handled datatype: " // dname // " from " // filename
endif

end function get_native_dtype


module procedure hdf_get_ndims
!! get rank or "ndims"
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:read: file handle is not open'

drank = -1

if (self%exist(dname)) then
  call h5ltget_dataset_ndims_f(self%lid, dname, drank, ier)
else
  write(stderr, *) 'ERROR:get_ndims: ' // dname // ' does not exist in ' // self%filename
endif

end procedure hdf_get_ndims


module procedure hdf_get_shape
!! must get dims before info, as "dims" must be allocated or segfault occurs.
integer(SIZE_T) :: dsize
integer :: dtype, drank, ier

if(.not.self%is_open) error stop 'h5fortran:get_shape: file handle is not open'

ier = -1

if (self%exist(dname)) then
  call h5ltget_dataset_ndims_f(self%lid, dname, drank, ier)
else
  write(stderr, *) 'ERROR:get_shape: ' // dname // ' does not exist in ' // self%filename
endif

if (ier == 0) then
  allocate(dims(drank))
  call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ier)
endif

if (present(ierr)) ierr = ier
if (ier /= 0) then
  if (present(ierr)) return
  error stop
endif

end procedure hdf_get_shape


module procedure hdf_get_chunk

integer :: ierr, drank
integer(HID_T) :: pid, did

if(.not.self%is_open) error stop 'h5fortran:read: file handle is not open'

chunk_size = -1
if (.not.self%exist(dname)) then
  write(stderr, *) 'ERROR:get_chunk: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

if(.not.self%is_chunked(dname)) return

call h5ltget_dataset_ndims_f(self%lid, dname, drank, ierr)
if (check(ierr, 'ERROR:get_chunk: get rank ' // dname // ' ' // self%filename)) return
call h5dopen_f(self%lid, dname, did, ierr)
if (check(ierr, 'ERROR:get_chunk: open dataset ' // dname // ' ' // self%filename)) return
call h5dget_create_plist_f(did, pid, ierr)
if (check(ierr, 'ERROR:get_chunk: get property list ID ' // dname // ' ' // self%filename)) return

call h5pget_chunk_f(pid, drank, chunk_size, ierr)
if (ierr /= drank) then
  write(stderr,*) 'ERROR:get_chunk read ' // dname // ' ' // self%filename
  return
endif

call h5dclose_f(did, ierr)
if (check(ierr, 'ERROR:get_chunk: close dataset: ' // dname // ' ' // self%filename)) return


end procedure hdf_get_chunk


module procedure hdf_get_layout

integer(HID_T) :: pid, did
integer :: ierr

if(.not.self%is_open) error stop 'h5fortran:read: file handle is not open'

layout = -1

if (.not.self%exist(dname)) then
  write(stderr, *) 'ERROR:get_layout: ' // dname // ' does not exist in ' // self%filename
  return
endif

call h5dopen_f(self%lid, dname, did, ierr)
if (check(ierr, 'ERROR:get_layout: open dataset ' // dname // ' ' // self%filename)) return
call h5dget_create_plist_f(did, pid, ierr)
if (check(ierr, 'ERROR:get_layout: get property list ID ' // dname // ' ' // self%filename)) return
call h5pget_layout_f(pid, layout, ierr)
if (check(ierr, 'ERROR:get_layout read ' // dname //' ' // self%filename)) return
call h5dclose_f(did, ierr)
if (check(ierr, 'ERROR:get_layout: close dataset: ' // dname //' ' // self%filename)) return

end procedure hdf_get_layout


module procedure hdf_check_exist

integer :: ierr

exists = .false.

if(.not.self%is_open) error stop 'h5fortran:exist: file handle is not open'

call h5ltpath_valid_f(self%lid, dname, .true., exists, ierr)
!! h5lexists_f can false error with groups--just use h5ltpath_valid

if (ierr/=0) then
  write(stderr,*) 'ERROR:h5fortran:check_exist: could not determine status of ' // dname // ' in ' // self%filename
  return
endif

end procedure hdf_check_exist



end submodule read
