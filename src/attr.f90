submodule (h5fortran) attr_smod

use hdf5, only : H5S_SCALAR_F, &
H5Aexists_by_name_f, H5Aopen_by_name_f, H5Aclose_f, H5Acreate_by_name_f, H5Adelete_f, H5Aget_space_f, &
H5Screate_f, H5Screate_simple_f, H5Sclose_f, &
H5Sget_simple_extent_dims_f, H5Sget_simple_extent_ndims_f, H5Sget_simple_extent_npoints_f, &
H5Tcopy_f, H5Tset_size_f, H5Tclose_f, &
H5Dopen_f, H5Dclose_f

implicit none (type, external)

contains


subroutine attr_shape_check(self, obj_name, attr_name, space_id, dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: space_id
integer(HSIZE_T), intent(in) :: dims(:)

integer :: ier
integer(HSIZE_T), dimension(size(dims)):: attr_dims, maxdims

call hdf_rank_check(self, obj_name // ":" // attr_name, space_id, size(dims))

call H5Sget_simple_extent_dims_f(space_id, attr_dims, maxdims, ier)
if (ier /= size(dims)) error stop 'ERROR:h5fortran:shape_check:H5Sget_simple_extent_dims: ' // obj_name // ":" // attr_name

if(.not. all(dims == attr_dims)) then
  write(stderr,*) 'ERROR:h5fortran:attr_shape_check: ' // obj_name // ':' // attr_name //': ', attr_dims,' /= ', dims
  error stop
endif

end subroutine attr_shape_check


subroutine attr_create(self, obj_name, attr_name, dtype, attr_dims, space_id, attr_id, dtype_id, charlen)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: attr_dims
integer(HID_T), intent(out) :: space_id, attr_id
integer(HID_T), intent(out), optional :: dtype_id
integer, intent(in), optional :: charlen !< length of character scalar

logical :: attr_exists
integer :: ier
integer(HID_T) :: type_id


if(dtype == H5T_NATIVE_CHARACTER) then
  if(.not. present(dtype_id)) error stop "ERROR:h5fortran:attr_create: character needs type_id"
  if(.not. present(charlen)) error stop "ERROR:h5fortran:attr_create: character type must specify charlen"
endif

if(.not.self%is_open()) error stop 'ERROR:h5fortran:attr_create: file handle is not open'

call H5Aexists_by_name_f(self%file_id, obj_name, attr_name, attr_exists, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attr_create:H5Aexists_by_name: " // obj_name // ":" // attr_name // ": " // self%filename

if(attr_exists) then
  !! unlike datasets, H5Awrite_f doesn't seem to handle overwrites. Errors result like "H5Oattribute.c line 918 in H5O__attr_write(): can't locate open attribute?"
  !! since attribute writes are whole dataset, so we workaround by deleting attribute and creating a new attribute of the same name.

  !! FIXME: assumes object is a dataset. How to detect this automatically?
  !! For now, user can manually delete attribute first if not a dataset.

  call attr_delete(self, obj_name, attr_name)
endif

!> create attribute dataspace
if(size(attr_dims) == 0) then
  call H5Screate_f(H5S_SCALAR_F, space_id, ier)
else
  call H5Screate_simple_f(size(attr_dims), attr_dims, space_id, ier)
endif
if (ier /= 0) error stop "ERROR:h5fortran:attr_create:h5screate:filespace " // obj_name // ":" // attr_name // ": " // self%filename

if(dtype == H5T_NATIVE_CHARACTER) then
  call h5tcopy_f(dtype, type_id, ier)
  if(ier /= 0) error stop "ERROR:h5fortran:attr_create:h5tcopy:character: " // obj_name // ":" // attr_name // ': ' // self%filename

  call h5tset_size_f(type_id, int(charlen, SIZE_T), ier)
  if(ier/=0) error stop "ERROR:h5fortran:attr_create:h5tset_size:char: " // obj_name // ":" // attr_name // ': ' // self%filename
  dtype_id = type_id
else
  type_id = dtype
endif

call H5Acreate_by_name_f(self%file_id, obj_name, attr_name, type_id, space_id, attr_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:attr_create:H5Acreate_by_name: " // obj_name // ":" // attr_name // ": " // self%filename

end subroutine attr_create


module procedure attr_delete
!! assumes object is a dataset

integer(HID_T) :: dset_id
integer :: ier

call H5Dopen_f(self%file_id, obj_name, dset_id, ier)
if (ier /= 0) error stop "ERROR:h5fortran:attr_delete:H5Dopen: " // obj_name // ": " // self%filename

call H5Adelete_f(dset_id, attr_name, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attr_delete:H5Adelete: " // obj_name // ":" // attr_name // ": " // self%filename

call H5Dclose_f(dset_id, ier)
if (ier /= 0) error stop "ERROR:h5fortran:attr_delete:H5Dclose: " // obj_name // ": " // self%filename

end procedure attr_delete


module procedure attr_exist

integer :: ier

call H5Aexists_by_name_f(self%file_id, obj_name, attr_name, attr_exist, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attr_create:H5Aexists_by_name: " // obj_name // ":" // attr_name // ": " // self%filename


end procedure attr_exist


end submodule attr_smod
