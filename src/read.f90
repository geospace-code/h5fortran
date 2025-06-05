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
H5T_DIR_ASCEND_F, &
H5I_ATTR_F, H5I_DATASET_F

use H5LT, only : h5ltpath_valid_f, h5ltget_dataset_ndims_f

implicit none

contains


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
