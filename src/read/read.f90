submodule (h5fortran) hdf5_read
!! This submodule is for reading HDF5 via submodules
use hdf5, only : h5dget_create_plist_f, &
  h5pget_layout_f, h5pget_chunk_f, &
  h5dget_type_f, h5tget_native_type_f, h5tget_class_f, H5Tget_order_f, h5tclose_f, h5tget_size_f, &
  H5T_DIR_ASCEND_F

use H5LT, only : h5ltpath_valid_f

implicit none (type, external)

contains


module procedure get_class

call get_dset_class(self, dname, get_class)

end procedure get_class


subroutine get_dset_class(self, dname, class, ds_id, size_bytes)
!! get the dataset class (integer, float, string, ...)
!! {H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F}
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(out) :: class
integer(hid_t), intent(in), optional :: ds_id
integer(size_t), intent(out), optional :: size_bytes

integer :: ierr
integer(hid_t) :: dtype_id, native_dtype_id, dset_id

if(present(ds_id)) then
  dset_id = ds_id
else
  call h5dopen_f(self%lid, dname, dset_id, ierr)
  if(ierr/=0) error stop 'h5fortran:get_class: ' // dname // ' from ' // self%filename
endif

call h5dget_type_f(dset_id, dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: dtype_id ' // dname // ' from ' // self%filename

call h5tget_native_type_f(dtype_id, H5T_DIR_ASCEND_F, native_dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: native_dtype_id ' // dname // ' from ' // self%filename

!> compose datatype inferred
call h5tget_class_f(native_dtype_id, class, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: class ' // dname // ' from ' // self%filename

if(present(size_bytes)) then
  call h5tget_size_f(native_dtype_id, size_bytes, ierr)
  if(ierr/=0) error stop 'h5fortran:get_class: byte size ' // dname // ' from ' // self%filename
endif

!> close to avoid memory leaks
call h5tclose_f(native_dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: closing native dtype ' // dname // ' from ' // self%filename

call h5tclose_f(dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: closing dtype ' // dname // ' from ' // self%filename

if(.not.present(ds_id)) then
  call h5dclose_f(dset_id, ierr)
  if(ierr/=0) error stop 'h5fortran:get_class: close dataset ' // dname // ' from ' // self%filename
endif

end subroutine get_dset_class


module procedure get_native_dtype
!! get the dataset variable type:
!! {H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE}

integer :: class
! integer :: order, machine_order
integer(size_t) :: size_bytes

call get_dset_class(self, dname, class, ds_id, size_bytes)

!> endianness and within type casting is handled by HDF5
! call h5tget_order_f(native_dtype_id, order, ierr)
! if(ierr/=0) error stop 'h5fortran:reader: get endianness ' // dname // ' from ' // self%filename
! !> check dataset endianness matches machine (in future, could swap endianness if needed)
! call h5tget_order_f(H5T_NATIVE_INTEGER, machine_order, ierr)
! if(order /= machine_order) error stop 'h5fortran:read: endianness /= machine native: ' &
! // dname // ' from ' // self%filename

if(class == H5T_INTEGER_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_INTEGER
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_STD_I64LE
  else
    error stop "h5fortran:get_native_dtype: expected 32-bit or 64-bit integer:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_FLOAT_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_REAL
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_NATIVE_DOUBLE
  else
    error stop "h5fortran:get_native_dtype: expected 32-bit or 64-bit real:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_STRING_F) then
  get_native_dtype = H5T_NATIVE_CHARACTER
else
  error stop "h5fortran:get_native_dtype: non-handled datatype: " // dname // " from " // self%filename
endif

end procedure get_native_dtype


module procedure hdf_get_ndim
!! get rank or "ndims"
integer :: ier

if(.not.self%is_open) error stop 'h5fortran:read: file handle is not open'

drank = -1

if (self%exist(dname)) then
  call h5ltget_dataset_ndims_f(self%lid, dname, drank, ier)
else
  write(stderr, '(a)') 'ERROR:get_ndim: ' // dname // ' does not exist in ' // self%filename
endif

end procedure hdf_get_ndim


module procedure hdf_get_shape
!! must get dims before info, as "dims" must be allocated or segfault occurs.
integer(SIZE_T) :: type_size
integer :: type_class, drank, ier

if(.not. self%exist(dname)) error stop 'h5fortran:get_shape: ' // dname // ' does not exist in ' // self%filename

call h5ltget_dataset_ndims_f(self%lid, dname, drank, ier)

if (ier == 0) then
  allocate(dims(drank))
  call h5ltget_dataset_info_f(self%lid, dname, dims=dims, &
    type_class=type_class, type_size=type_size, errcode=ier)
endif

if (ier /= 0) error stop "h5fortran:get_shape: could not get info: " // dname // ' from ' // self%filename

end procedure hdf_get_shape


module procedure hdf_get_chunk

integer :: ierr, drank
integer(HID_T) :: pid, dset_id

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
call h5dopen_f(self%lid, dname, dset_id, ierr)
if (check(ierr, 'ERROR:get_chunk: open dataset ' // dname // ' ' // self%filename)) return
call h5dget_create_plist_f(dset_id, pid, ierr)
if (check(ierr, 'ERROR:get_chunk: get property list ID ' // dname // ' ' // self%filename)) return

call h5pget_chunk_f(pid, drank, chunk_size, ierr)
if (ierr /= drank) then
  write(stderr,*) 'ERROR:get_chunk read ' // dname // ' ' // self%filename
  return
endif

call h5dclose_f(dset_id, ierr)
if (check(ierr, 'ERROR:get_chunk: close dataset: ' // dname // ' ' // self%filename)) return

end procedure hdf_get_chunk


module procedure hdf_get_layout

integer(HID_T) :: pid, dset_id
integer :: ierr

if(.not.self%is_open) error stop 'h5fortran:read: file handle is not open'

layout = -1

if (.not.self%exist(dname)) then
  write(stderr, *) 'ERROR:get_layout: ' // dname // ' does not exist in ' // self%filename
  return
endif

call h5dopen_f(self%lid, dname, dset_id, ierr)
if (check(ierr, 'ERROR:get_layout: open dataset ' // dname // ' ' // self%filename)) return
call h5dget_create_plist_f(dset_id, pid, ierr)
if (check(ierr, 'ERROR:get_layout: get property list ID ' // dname // ' ' // self%filename)) return
call h5pget_layout_f(pid, layout, ierr)
if (check(ierr, 'ERROR:get_layout read ' // dname //' ' // self%filename)) return
call h5dclose_f(dset_id, ierr)
if (check(ierr, 'ERROR:get_layout: close dataset: ' // dname //' ' // self%filename)) return

end procedure hdf_get_layout


module procedure hdf_check_exist

integer :: ierr

if(.not.self%is_open) error stop 'h5fortran:exist: file handle is not open'

call h5ltpath_valid_f(self%lid, dname, .true., hdf_check_exist, ierr)
!! h5lexists_f can false error with groups--just use h5ltpath_valid

if (ierr/=0) error stop 'h5fortran:check_exist: could not determine status of ' // dname // ' in ' // self%filename

end procedure hdf_check_exist



end submodule hdf5_read
