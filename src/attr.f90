submodule (h5fortran) attr_smod

use hdf5, only : H5S_SCALAR_F, &
H5Aexists_by_name_f, H5Aopen_by_name_f, H5Aclose_f, H5Acreate_by_name_f, H5Adelete_f, H5Aget_space_f, &
H5Screate_f, H5Screate_simple_f, H5Sclose_f, &
H5Sget_simple_extent_dims_f, H5Sget_simple_extent_ndims_f, H5Sget_simple_extent_npoints_f, &
H5Tcopy_f, H5Tset_size_f, H5Tclose_f, &
H5Dopen_f, H5Dclose_f

implicit none (type, external)

contains


subroutine attr_create(self, obj_name, attr_name, dtype, attr_dims, attr_id, dtype_id, charlen)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: attr_dims
integer(HID_T), intent(out) :: attr_id, dtype_id
integer, intent(in), optional :: charlen !< length of character scalar

logical :: attr_exists
integer :: ier
integer(HID_T) :: space_id

call H5Tcopy_f(dtype, dtype_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attr_create:H5Tcopy: " // obj_name // ":" // attr_name // ': ' // self%filename


if(dtype == H5T_NATIVE_CHARACTER) then
  if(.not. present(charlen)) error stop "ERROR:h5fortran:attr_create: character type must specify charlen"

  call H5Tset_size_f(dtype_id, int(charlen, SIZE_T), ier)
  if(ier/=0) error stop "ERROR:h5fortran:attr_create:h5tset_size:char: " // obj_name // ":" // attr_name // ': ' // self%filename
endif


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

call H5Acreate_by_name_f(self%file_id, obj_name, attr_name, dtype_id, space_id, attr_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:attr_create:H5Acreate_by_name: " // obj_name // ":" // attr_name // ": " // self%filename

call H5Sclose_f(space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writeattr:H5Sclose " // obj_name // ":" // attr_name // " in " // self%filename

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
