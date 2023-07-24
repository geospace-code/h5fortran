submodule (h5fortran) attr_smod

use hdf5, only : H5S_SCALAR_F, &
H5Aexists_by_name_f, H5Aopen_by_name_f, H5Aclose_f, H5Acreate_by_name_f, H5Adelete_f, H5Aget_space_f, &
H5Screate_f, H5Screate_simple_f, H5Sclose_f, &
H5Sget_simple_extent_dims_f, H5Sget_simple_extent_ndims_f, &
H5Tcopy_f, H5Tset_size_f, H5Tclose_f, &
H5Dopen_f, H5Dclose_f

implicit none

contains


subroutine attr_create(self, obj_name, attr_name, dtype, attr_dims, attr_id, dtype_id, charlen)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: attr_dims
integer(HID_T), intent(out) :: attr_id, dtype_id
integer, intent(in), optional :: charlen !< length of character scalar

integer :: ier
integer(HID_T) :: space_id

call H5Tcopy_f(dtype, dtype_id, ier)
call estop(ier, "attr_create:H5Tcopy", self%filename, obj_name, attr_name)


if(dtype == H5T_NATIVE_CHARACTER) then
  if(.not. present(charlen)) error stop "ERROR:h5fortran:attr_create: character type must specify charlen"
  if (charlen < 1) error stop "ERROR:h5fortran:attr_create: character type must specify charlen > 0"

  call H5Tset_size_f(dtype_id, int(charlen, SIZE_T), ier)
  call estop(ier, "attr_create:H5Aset_size", self%filename, obj_name, attr_name)
endif

if(self % exist_attr(obj_name, attr_name)) then
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
call estop(ier, "attr_create:H5Screate", self%filename, obj_name, attr_name)

call H5Acreate_by_name_f(self%file_id, obj_name, attr_name, dtype_id, space_id, attr_id, ier)
call estop(ier, "attr_create:H5Acreate_by_name", self%filename, obj_name, attr_name)

call H5Sclose_f(space_id, ier)
call estop(ier, "attr_create:H5Aclose", self%filename, obj_name, attr_name)

end subroutine attr_create


module procedure attr_delete
!! assumes object is a dataset

integer(HID_T) :: dset_id
integer :: ier

call H5Dopen_f(self%file_id, obj_name, dset_id, ier)
call estop(ier, "attr_delete:H5Dopen", self%filename, obj_name, attr_name)

call H5Adelete_f(dset_id, attr_name, ier)
call estop(ier, "attr_delete:H5Adelete", self%filename, obj_name, attr_name)

call H5Dclose_f(dset_id, ier)
call estop(ier, "attr_delete:H5Dclose", self%filename, obj_name, attr_name)

end procedure attr_delete


module procedure attr_exist

integer :: ier

attr_exist = self%exist(obj_name)
if(.not. attr_exist) return

call H5Aexists_by_name_f(self%file_id, obj_name, attr_name, attr_exist, ier)
call estop(ier, "attr_exist:H5Aexists_by_name", self%filename, obj_name, attr_name)

end procedure attr_exist


end submodule attr_smod
