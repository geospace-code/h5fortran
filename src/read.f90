submodule (h5fortran) hdf5_read

use, intrinsic:: iso_c_binding, only : c_null_char

use hdf5, only : &
H5Aget_space_f, H5Aget_type_f, H5Aopen_by_name_f, H5Aclose_f, H5Aget_storage_size_f, &
h5pget_layout_f, h5pget_chunk_f, h5pclose_f, h5pget_nfilters_f, h5pget_filter_f, &
H5Dget_create_plist_f, h5dget_type_f, h5dopen_f, h5dclose_f, H5Dget_space_f, H5Dget_storage_size_f, &
H5Iget_type_f, &
h5lexists_f, &
H5Sget_simple_extent_ndims_f, H5Sget_simple_extent_dims_f, H5Sget_simple_extent_npoints_f, H5Sclose_f, &
h5tclose_f, h5tget_native_type_f, h5tget_class_f, H5Tget_order_f, h5tget_size_f, h5tget_strpad_f, H5Tis_variable_str_f, &
h5z_filter_deflate_f, &
H5T_DIR_ASCEND_F, &
H5I_ATTR_F, H5I_DATASET_F

use H5LT, only : h5ltpath_valid_f, h5ltget_dataset_ndims_f

implicit none

contains


module procedure get_class

integer(HID_T) :: dset_id
integer :: ier

call H5Dopen_f(self%file_id, dname, dset_id, ier)
call estop(ier, "get_class:H5Dopen", self%filename, dname)

call get_obj_class(self, dname, dset_id, get_class)

call H5Dclose_f(dset_id, ier)
call estop(ier, "get_class:H5Dclose", self%filename, dname)

end procedure get_class


module procedure get_strpad
!! H5T_STR_NULLTERM  Null terminate (as C does).
!! H5T_STR_NULLPAD   Pad with zeros.
!! H5T_STR_SPACEPAD  Pad with spaces (as FORTRAN does).

integer :: class
integer(HID_T) :: dset_id
integer :: ier

call H5Dopen_f(self%file_id, dset_name, dset_id, ier)
call estop(ier, "get_strpad:H5Dopen", self%filename, dset_name)

call get_obj_class(self, dset_name, dset_id, class, pad_type=get_strpad)

call H5Dclose_f(dset_id, ier)
call estop(ier, "get_strpad:H5Dclose", self%filename, dset_name)

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
call H5Dopen_f(self%file_id, dname, dset_id, ier)
call estop(ier, "get_deflate:H5Dopen", self%filename, dname)

call h5dget_create_plist_f(dset_id, dcpl, ier)
call estop(ier, "get_deflate:H5Dget_create_plist", self%filename, dname)

call H5Dclose_f(dset_id, ier)
call estop(ier, "get_deflate:H5Dclose", self%filename, dname)

call h5pget_nfilters_f(dcpl, Nf, ier)
call estop(ier, "get_deflate:H5Pget_nfilters", self%filename, dname)

filters: do i = 1, Nf
  filter_name = ""

  call h5pget_filter_f(dcpl, i, &
  flags, &
  Naux, Aux, &
  len(filter_name, SIZE_T), filter_name, &
  filter_id, ier)
  call estop(ier, "get_deflate:H5Pget_filter", self%filename, dname)
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
call estop(ier, "get_obj_class:H5Iget_type", self%filename, obj_name)

if(obj_type == H5I_DATASET_F) then
  call H5Dget_type_f(obj_id, obj_dtype, ier)
elseif(obj_type == H5I_ATTR_F) then
  call H5Aget_type_f(obj_id, obj_dtype, ier)
else
  error stop "ERROR:h5fortran:get_obj_class: only datasets and attributes have datatype " // obj_name // " " // self%filename
endif
call estop(ier, "get_obj_class:H5[A,D]get_type", self%filename, obj_name)

call H5Tget_native_type_f(obj_dtype, H5T_DIR_ASCEND_F, native_dtype, ier)
call estop(ier, "get_obj_class:H5Tget_native_type", self%filename, obj_name)

!> compose datatype inferred
call H5Tget_class_f(native_dtype, class, ier)
call estop(ier, "get_obj_class:H5Tget_class", self%filename, obj_name)

if(present(size_bytes)) then
  call H5Tget_size_f(native_dtype, size_bytes, ier)
  call estop(ier, "get_obj_class:H5Tget_size", self%filename, obj_name)
endif

call H5Tclose_f(native_dtype, ier)
call estop(ier, "get_obj_class:H5Tclose", self%filename, obj_name)

if(present(pad_type)) then
  if(class /= H5T_STRING_F) error stop "ERROR:h5fortran:get_class: pad_type only for string"

  call H5Tget_strpad_f(obj_dtype, pad_type, ier)
  call estop(ier, "get_obj_class:H5Tget_strpad", self%filename, obj_name)
endif

call H5Tclose_f(obj_dtype, ier)
call estop(ier, "get_obj_class:H5Tclose", self%filename, obj_name)

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
  call estop(ier, "get_native_dtype:H5Dopen", self%filename, dname)
endif

call get_obj_class(self, dname, o_id, class, size_bytes=size_bytes)

if(.not.present(obj_id)) then
  call H5Dclose_f(o_id, ier)
  call estop(ier, "get_native_dtype:H5Dclose", self%filename, dname)
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

call H5LTget_dataset_ndims_f(self%file_id, dname, drank, ier)
call estop(ier, "get_ndim:H5LTget_dataset_ndims", self%filename, dname)

end procedure hdf_get_ndim


module procedure hdf_get_shape

integer :: drank, ier
integer(HID_T) :: obj_id, space_id
integer(HSIZE_T), allocatable :: maxdims(:)


if(.not. self%exist(obj_name)) error stop 'ERROR:h5fortran:get_shape: ' // obj_name // ' does not exist in ' // self%filename

if(present(attr_name)) then
  call H5Aopen_by_name_f(self%file_id, obj_name, attr_name, obj_id, ier)
  call estop(ier, "get_shape:H5Aopen_by_name", self%filename, obj_name)
  call H5Aget_space_f(obj_id, space_id, ier)
  call estop(ier, "get_shape:H5Aget_space", self%filename, obj_name)
else
  call H5Dopen_f(self%file_id, obj_name, obj_id, ier)
  call estop(ier, "get_shape:H5Dopen", self%filename, obj_name)
  call H5Dget_space_f(obj_id, space_id, ier)
  call estop(ier, "get_shape:H5Dget_space", self%filename, obj_name)
endif



call H5Sget_simple_extent_ndims_f(space_id, drank, ier)
call estop(ier, "get_shape:H5Sget_simple_extent_ndims", self%filename, obj_name)

allocate(dims(drank), maxdims(drank))
call H5Sget_simple_extent_dims_f(space_id, dims, maxdims, ier)
if (ier /= drank) error stop 'ERROR:h5fortran:get_shape:H5Sget_simple_extent_dims: ' // obj_name // ' in ' // self%filename

call H5Sclose_f(space_id, ier)
call estop(ier, "get_shape:H5Sclose", self%filename, obj_name)

if(present(attr_name)) then
  call H5Aclose_f(obj_id, ier)
  call estop(ier, "get_shape:H5Aclose", self%filename, obj_name)
else
  call H5Dclose_f(obj_id, ier)
  call estop(ier, "get_shape:H5Dclose", self%filename, obj_name)
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
  call estop(ier, "get_chunk:H5Dopen", self%filename, dname)

  call H5Dget_space_f(dset_id, space_id, ier)
  call estop(ier, "get_chunk:H5Dget_space", self%filename, dname)
  call H5Sget_simple_extent_ndims_f(space_id, drank, ier)
  call estop(ier, "get_chunk:H5Sget_simple_extent_ndims", self%filename, dname)
  call H5Sclose_f(space_id, ier)
  call estop(ier, "get_chunk:H5Sclose", self%filename, dname)

  call h5dget_create_plist_f(dset_id, dapl, ier)
  call estop(ier, "get_chunk:H5Dget_create_plist", self%filename, dname)

  call h5dclose_f(dset_id, ier)
  call estop(ier, "get_chunk:H5Dclose", self%filename, dname)

  call h5pget_chunk_f(dapl, drank, cs, ier)
  if (ier /= drank) error stop 'ERROR:h5fortran:get_chunk:h5pget_chunk ' // dname // ' ' // self%filename
  !! yes ier == drank is success for this call

  call h5pclose_f(dapl, ier)
  call estop(ier, "get_chunk:H5Pclose", self%filename, dname)
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
call estop(ier, "get_layout:H5Dopen", self%filename, dname)

call h5dget_create_plist_f(dset_id, dapl, ier)
call estop(ier, "get_layout:H5Dget_create_plist", self%filename, dname)

call h5dclose_f(dset_id, ier)
call estop(ier, "get_layout:H5Dclose", self%filename, dname)

call h5pget_layout_f(dapl, layout, ier)
call estop(ier, "get_layout:H5Pget_layout", self%filename, dname)

call h5pclose_f(dapl, ier)
call estop(ier, "get_layout:H5Pclose", self%filename, dname)

end procedure hdf_get_layout


module procedure hdf_check_exist

integer :: ier

if(.not. self%is_open()) error stop 'ERROR:h5fortran:exist: file handle is not open: ' // self%filename

if(len_trim(obj_name) == 0) error stop "ERROR:h5fortran:check_exist: object name must not be empty"

call h5ltpath_valid_f(self%file_id, obj_name, .true., hdf_check_exist, ier)
!! h5lexists_f can false error with groups--just use h5ltpath_valid

call estop(ier, "check_exist:H5LTpath_valid", self%filename, obj_name)

end procedure hdf_check_exist


end submodule hdf5_read
