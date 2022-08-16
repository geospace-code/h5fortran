submodule (h5fortran) hdf5_read

use, intrinsic:: iso_c_binding, only : c_null_char

use hdf5, only : &
H5Aget_space_f, H5Aget_type_f, H5Aopen_by_name_f, H5Aclose_f, &
h5pget_layout_f, h5pget_chunk_f, h5pclose_f, h5pget_nfilters_f, h5pget_filter_f, &
H5Dget_create_plist_f, h5dget_type_f, h5dopen_f, h5dclose_f, H5Dget_space_f, &
H5Iget_type_f, &
h5lexists_f, &
H5Sget_simple_extent_ndims_f, H5Sget_simple_extent_dims_f, H5Sclose_f, &
h5tclose_f, h5tget_native_type_f, h5tget_class_f, H5Tget_order_f, h5tget_size_f, h5tget_strpad_f, &
h5z_filter_deflate_f, &
H5T_DIR_ASCEND_F, &
H5I_ATTR_F, H5I_DATASET_F

use H5LT, only : h5ltpath_valid_f, h5ltget_dataset_ndims_f

implicit none (type, external)

contains


module procedure get_class

integer(HID_T) :: dset_id
integer :: ier

call H5Dopen_f(self%file_id, dname, dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:get_class:H5Dopen " // dname // " " // self%filename

call get_obj_class(self, dname, dset_id, get_class)

call H5Dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:get_class:H5Dclose " // dname // " " // self%filename

end procedure get_class


module procedure get_strpad
!! H5T_STR_NULLTERM  Null terminate (as C does).
!! H5T_STR_NULLPAD   Pad with zeros.
!! H5T_STR_SPACEPAD  Pad with spaces (as FORTRAN does).

integer :: class
integer(HID_T) :: dset_id
integer :: ier

call H5Dopen_f(self%file_id, dset_name, dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:get_strpad:H5Dopen " // dset_name // " " // self%filename

call get_obj_class(self, dset_name, dset_id, class, pad_type=get_strpad)

call H5Dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:get_strpad:H5Dclose " // dset_name // " " // self%filename

end procedure get_strpad


module procedure get_deflate
!! h5pget_filter_f doesn't work collectively, will crash on h5fclose_f
!! if(mpi_id==0) with mpi_bcast does not work, same crash.
!! better to use H5Pall_filters_avail_f when mpi=.true.

integer :: i, j, ier
integer :: flags !< bit pattern
integer(HID_T) :: dcpl, dset_id
integer(SIZE_T) :: Naux
integer :: Aux(8)  !< arbitrary length
integer :: Nf, filter_id
character(32) :: filter_name

logical :: debug = .false.


get_deflate = .false.

Naux = size(Aux, kind=SIZE_T)

if(.not.self%exist(dname)) error stop "ERROR:h5fortran:get_deflate: " // dname // " does not exist: " // self%filename
call h5dopen_f(self%file_id, dname, dset_id, ier)
if (ier/=0) error stop "ERROR:h5fortran:get_deflate:h5dopen: " // dname // " in " // self%filename

call h5dget_create_plist_f(dset_id, dcpl, ier)
if (ier/=0) error stop "ERROR:h5fortran:get_deflate:h5dget_create_plist: " // dname // " in " // self%filename

call h5dclose_f(dset_id, ier)
if (ier/=0) error stop "ERROR:h5fortran:get_deflate:h5dclose: " // dname // " in " // self%filename

call h5pget_nfilters_f(dcpl, Nf, ier)
if (ier/=0) error stop "ERROR:h5fortran:get_deflate:h5pget_nfilters: " // dname // " in " // self%filename

filters: do i = 1, Nf
  filter_name = ""

  call h5pget_filter_f(dcpl, i, &
  flags, &
  Naux, Aux, &
  len(filter_name, SIZE_T), filter_name, &
  filter_id, ier)
  if(ier/=0) error stop "ERROR:h5fortran:get_deflate:h5pget_filter: " // dname // " in " // self%filename
  if(filter_id < 0) write(stderr,'(a,i0)') "ERROR:h5fortran:get_deflate:h5pget_filter: index error " // dname, i

  if (debug) then
    j = index(filter_name, c_null_char)
    if(j>0) print *, "TRACE:get_filter: filter name: ", filter_name(:j-1)
  endif

  get_deflate = filter_id == H5Z_FILTER_DEFLATE_F
  if(get_deflate) exit filters

end do filters

call h5pclose_f(dcpl, ier)

end procedure get_deflate


module procedure get_obj_class

integer :: ier, obj_type
integer(HID_T) :: obj_dtype, native_dtype

call H5Iget_type_f(obj_id, obj_type, ier)
if(ier /= 0) error stop "ERROR:h5fortran:get_obj_class:H5Iget_type: " // obj_name // " " // self%filename

if(obj_type == H5I_DATASET_F) then
  call H5Dget_type_f(obj_id, obj_dtype, ier)
elseif(obj_type == H5I_ATTR_F) then
  call H5Aget_type_f(obj_id, obj_dtype, ier)
else
  error stop "ERROR:h5fortran:get_obj_class: only datasets and attributes have datatype " // obj_name // " " // self%filename
endif
if(ier/=0) error stop 'ERROR:h5fortran:get_class: obj_dtype ' // obj_name // ' from ' // self%filename

call H5Tget_native_type_f(obj_dtype, H5T_DIR_ASCEND_F, native_dtype, ier)
if(ier/=0) error stop 'ERROR:h5fortran:get_class: native_dtype ' // obj_name // ' from ' // self%filename

!> compose datatype inferred
call H5Tget_class_f(native_dtype, class, ier)
if(ier/=0) error stop 'ERROR:h5fortran:get_class: class ' // obj_name // ' from ' // self%filename

if(present(size_bytes)) then
  call H5Tget_size_f(native_dtype, size_bytes, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_class: byte size ' // obj_name // ' from ' // self%filename
endif

call H5Tclose_f(native_dtype, ier)
if(ier/=0) error stop 'ERROR:h5fortran:get_class: closing native dtype ' // obj_name // ' from ' // self%filename

if(present(pad_type)) then
  if(class /= H5T_STRING_F) error stop "ERROR:h5fortran:get_class: pad_type only for string"

  call H5Tget_strpad_f(obj_dtype, pad_type, ier)
  if(ier /= 0) error stop "h5fortran:read:h5tget_strpad " // obj_name // " in " // self%filename
endif

call H5Tclose_f(obj_dtype, ier)
if(ier/=0) error stop 'ERROR:h5fortran:get_class: closing dtype ' // obj_name // ' from ' // self%filename

end procedure get_obj_class


module procedure get_native_dtype
!! get the dataset variable type:
!! {H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE}

integer :: class, ier
! integer :: order, machine_order
integer(size_t) :: size_bytes
integer(HID_T) :: o_id

if(present(obj_id)) then
  o_id = obj_id
else
  !! assume dataset
  call H5Dopen_f(self%file_id, dname, o_id, ier)
  if(ier /= 0) error stop "ERROR:h5fortran:get_native_dtype: H5Dopen " // dname // " in " // self%filename
endif

call get_obj_class(self, dname, o_id, class, size_bytes=size_bytes)

if(.not.present(obj_id)) then
  call H5Dclose_f(o_id, ier)
  if(ier /= 0) error stop "ERROR:h5fortran:get_native_dtype: H5Dclose " // dname // " in " // self%filename
endif

!> endianness and within type casting is handled by HDF5
! call h5tget_order_f(native_dtype, order, ier)
! if(ier/=0) error stop 'ERROR:h5fortran:reader: get endianness ' // dname // ' from ' // self%filename
! !> check dataset endianness matches machine (in future, could swap endianness if needed)
! call h5tget_order_f(H5T_NATIVE_INTEGER, machine_order, ier)
! if(order /= machine_order) error stop 'ERROR:h5fortran:read: endianness /= machine native: ' &
! // dname // ' from ' // self%filename

if(class == H5T_INTEGER_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_INTEGER
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_STD_I64LE
  else
    error stop "ERROR:h5fortran:get_native_dtype: expected 32-bit or 64-bit integer:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_FLOAT_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_REAL
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_NATIVE_DOUBLE
  else
    error stop "ERROR:h5fortran:get_native_dtype: expected 32-bit or 64-bit real:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_STRING_F) then
  get_native_dtype = H5T_NATIVE_CHARACTER
else
  error stop "ERROR:h5fortran:get_native_dtype: non-handled datatype: " // dname // " from " // self%filename
endif

end procedure get_native_dtype


module procedure hdf_get_ndim
!! get rank or "ndims"
integer :: ier

if (.not. self%exist(dname)) error stop 'ERROR:h5fortran:get_ndim: ' // dname // ' does not exist in ' // self%filename

call h5ltget_dataset_ndims_f(self%file_id, dname, drank, ier)
if(ier/=0) error stop 'ERROR:h5fortran:get_ndim:h5ltget_dataset_ndims ' // dname // ' from ' // self%filename

end procedure hdf_get_ndim


module procedure hdf_get_shape

integer :: drank, ier
integer(HID_T) :: obj_id, space_id
integer(HSIZE_T), allocatable :: maxdims(:)


if(.not. self%exist(obj_name)) error stop 'ERROR:h5fortran:get_shape: ' // obj_name // ' does not exist in ' // self%filename

if(present(attr_name)) then
  call H5Aopen_by_name_f(self%file_id, obj_name, attr_name, obj_id, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_shape:H5Aopen ' // obj_name // ":" // attr_name // ' from ' // self%filename
  call H5Aget_space_f(obj_id, space_id, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_shape:H5Aget_space ' // obj_name // ":" // attr_name // ' from ' // self%filename
else
  call H5Dopen_f(self%file_id, obj_name, obj_id, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_shape:H5Dopen ' // obj_name // ' from ' // self%filename
  call H5Dget_space_f(obj_id, space_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:get_shape:H5Dget_space ' // obj_name // ' from ' // self%filename
endif



call H5Sget_simple_extent_ndims_f(space_id, drank, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:get_shape:H5Sget_simple_extent_ndims: ' // obj_name // ' in ' // self%filename

allocate(dims(drank), maxdims(drank))
call H5Sget_simple_extent_dims_f(space_id, dims, maxdims, ier)
if (ier /= drank) error stop 'ERROR:h5fortran:get_shape:H5Sget_simple_extent_dims: ' // obj_name // ' in ' // self%filename

call H5Sclose_f(space_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:get_shape:H5Sclose: ' // obj_name // ' in ' // self%filename

if(present(attr_name)) then
  call H5Aclose_f(obj_id, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_shape:H5Aclose: ' // obj_name // ' in ' // self%filename
else
  call H5Dclose_f(obj_id, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_shape:H5Dclose: ' // obj_name // ' in ' // self%filename
endif

end procedure hdf_get_shape


module procedure hdf_get_chunk

integer :: ier, drank
integer(HID_T) :: dapl, dset_id, space_id
integer(HSIZE_T) :: cs(size(chunk_size))

cs = -1

if (.not.self%exist(dname)) error stop 'ERROR:h5fortran:get_chunk: ' // dname // ' does not exist in ' // self%filename

if(self%is_chunked(dname)) then
  call H5Dopen_f(self%file_id, dname, dset_id, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_chunk:H5Dopen ' // dname // ' from ' // self%filename

  call H5Dget_space_f(dset_id, space_id, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_chunk:H5Dget_space ' // dname // ' from ' // self%filename
  call H5Sget_simple_extent_ndims_f(space_id, drank, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:get_chunk:H5Sget_simple_extent_ndims: ' // dname // ' in ' // self%filename
  call H5Sclose_f(space_id, ier)
  if(ier/=0) error stop 'ERROR:h5fortran:get_chunk:H5Sclose: ' // dname // ' in ' // self%filename

  call h5dget_create_plist_f(dset_id, dapl, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:get_chunk: get property list ID ' // dname // ' ' // self%filename

  call h5dclose_f(dset_id, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:get_chunk: close dataset: ' // dname // ' ' // self%filename

  call h5pget_chunk_f(dapl, drank, cs, ier)
  if (ier /= drank) error stop 'ERROR:h5fortran:get_chunk:h5pget_chunk ' // dname // ' ' // self%filename
  !! yes ier == drank is success for this call

  call h5pclose_f(dapl, ier)
  if (ier /= 0) error stop 'ERROR:h5fortran:get_chunk: close property list ' // dname // ' ' // self%filename
endif

select type (chunk_size)
type is (integer(HSIZE_T))
  chunk_size = cs
type is (integer(int32))
  chunk_size = int(cs)
class default
  error stop 'ERROR:h5fortran:get_chunk: unknown type for chunk_size'
end select

end procedure hdf_get_chunk


module procedure hdf_get_layout

integer(HID_T) :: dapl, dset_id
integer :: ier

layout = -1

if (.not. self%exist(dname)) error stop 'ERROR:h5fortran:get_layout: ' // dname // ' does not exist in ' // self%filename

call h5dopen_f(self%file_id, dname, dset_id, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:get_layout: open dataset ' // dname // ' ' // self%filename

call h5dget_create_plist_f(dset_id, dapl, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:get_layout: get property list ID ' // dname // ' ' // self%filename

call h5dclose_f(dset_id, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:get_layout: close dataset: ' // dname //' ' // self%filename

call h5pget_layout_f(dapl, layout, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:get_layout read ' // dname //' ' // self%filename

call h5pclose_f(dapl, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:get_chunk: close property list ' // dname // ' ' // self%filename

end procedure hdf_get_layout


module procedure hdf_check_exist

integer :: ier

if(.not. self%is_open()) error stop 'ERROR:h5fortran:exist: file handle is not open: ' // self%filename

call h5ltpath_valid_f(self%file_id, dname, .true., hdf_check_exist, ier)
!! h5lexists_f can false error with groups--just use h5ltpath_valid

if (ier/=0) error stop 'ERROR:h5fortran:check_exist: could not determine status of ' // dname // ' in ' // self%filename

end procedure hdf_check_exist


end submodule hdf5_read
