submodule (h5fortran) attr_smod

use hdf5, only : H5S_SCALAR_F, &
H5Aexists_by_name_f, H5Aopen_by_name_f, H5Awrite_f, H5Aclose_f, H5Acreate_by_name_f, &
H5Screate_f, H5Screate_simple_f, H5Sclose_f, &
H5Tcopy_f, H5Tset_size_f, H5Tclose_f

use h5lt, only : h5ltget_attribute_ndims_f, h5ltget_attribute_info_f

implicit none (type, external)

contains


subroutine attr_rank_check(self, dset_name, attr_name, mrank, vector_scalar)
!! check for matching rank, else bad reads can occur--doesn't always crash without this check

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dset_name, attr_name
integer, intent(in) :: mrank
logical, intent(out), optional :: vector_scalar

integer(HSIZE_T) :: attr_dims(1)
integer(SIZE_T) :: attr_bytes
integer :: ierr, attr_rank, attr_type
logical :: attr_exists

if(present(vector_scalar)) vector_scalar = .false.

if(.not.self%is_open()) error stop 'ERROR:h5fortran:attr_rank_check: file handle is not open'

call H5Aexists_by_name_f(self%file_id, dset_name, attr_name, attr_exists, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:attr_rank_check:H5Aexists_by_name_f failed: " // dset_name // ":" // attr_name
if(.not.attr_exists) error stop 'ERROR:h5fortran:attr_rank_check: attribute not exist: ' // dset_name // ":" // attr_name

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_attribute_ndims_f(self%file_id, dset_name, attr_name, attr_rank, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:attr_rank__check:get_attribute_ndims: ' // dset_name // ":" // attr_name


if (attr_rank == mrank) return

if (present(vector_scalar) .and. attr_rank == 1 .and. mrank == 0) then
  !! check if vector of length 1
  call h5ltget_attribute_info_f(self%file_id, dset_name, attr_name, attr_dims, attr_type, attr_bytes, ierr)
  if (ierr/=0) error stop 'ERROR:h5fortran:attr_rank_check:get_dataset_info ' // dset_name // ":" // attr_name
  if (attr_dims(1) == 1) then
    vector_scalar = .true.
    return
  endif
endif

write(stderr,'(A,I0,A,I0)') 'ERROR:h5fortran:attr_rank_check: rank mismatch ' // dset_name // ":" // attr_name // &
  ' = ', attr_rank,'  variable rank =', mrank
error stop

end subroutine attr_rank_check


subroutine attr_shape_check(self, dset_name, attr_name, dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dset_name, attr_name
integer(HSIZE_T), intent(in) :: dims(:)

integer :: attr_type, ierr
integer(SIZE_T) :: attr_bytes
integer(HSIZE_T), dimension(size(dims)):: attr_dims

call attr_rank_check(self, dset_name, attr_name, size(dims))

!> check for matching size, else bad reads can occur.
call h5ltget_attribute_info_f(self%file_id, dset_name, attr_name, attr_dims, attr_type, attr_bytes, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:attr_shape_check:get_attribute_info' // dset_name // ':' // attr_name

if(.not. all(dims == attr_dims)) then
  write(stderr,*) 'ERROR:h5fortran:attr_shape_check: ' // dset_name // ':' // attr_name //': ', attr_dims,' /= ', dims
  error stop
endif

end subroutine attr_shape_check


subroutine attr_create(self, dname, attr, dtype, mem_dims, attr_id, dtype_id, charlen)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: mem_dims
integer(HID_T), intent(out) :: attr_id
integer(HID_T), intent(out), optional :: dtype_id
integer, intent(in), optional :: charlen !< length of character scalar

logical :: attr_exists
integer :: ier
integer(HID_T) :: filespace_id, type_id


if(dtype == H5T_NATIVE_CHARACTER) then
  if(.not. present(dtype_id)) error stop "ERROR:h5fortran:attr_create: character needs type_id"
  if(.not. present(charlen)) error stop "ERROR:h5fortran:attr_create: character type must specify charlen"
endif

if(.not.self%is_open()) error stop 'h5fortran:writeattr: file handle is not open'

call H5Aexists_by_name_f(self%file_id, dname, attr, attr_exists, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attrwrite:H5Aexists_by_name failed: " // dname // ":" // attr // " in " // self%filename

if(attr_exists) then

  if (size(mem_dims) == 0) then
    !! scalar
    call attr_rank_check(self, dname, attr, size(mem_dims))
  else
    call attr_shape_check(self, dname, attr, mem_dims)
  endif

  call H5Aopen_by_name_f(self%file_id, dname, attr, attr_id, ier)
  if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name failed: " // dname // ":" // attr // " in " // self%filename

  if(dtype == H5T_NATIVE_CHARACTER) then
    call h5tcopy_f(dtype, type_id, ier)
    if(ier /= 0) error stop "h5fortran:h5tcopy:character: " // dname // ' in ' // self%filename

    call h5tset_size_f(type_id, int(charlen, SIZE_T), ier)
    if(ier /= 0) error stop "h5fortran:h5tset_size:character: " // dname // ' in ' // self%filename

    dtype_id = type_id
  endif

  return
endif

!> create attribute dataspace
if(size(mem_dims) == 0) then
  call H5Screate_f(H5S_SCALAR_F, filespace_id, ier)
else
  call H5Screate_simple_f(size(mem_dims), mem_dims, filespace_id, ier)
endif
if (ier /= 0) error stop "ERROR:h5fortran:hdf_create:h5screate:filespace " // dname // " in " // self%filename

if(dtype == H5T_NATIVE_CHARACTER) then
  call h5tcopy_f(dtype, type_id, ier)
  if(ier /= 0) error stop "h5fortran:h5tcopy:character: " // dname // ' in ' // self%filename

  call h5tset_size_f(type_id, int(charlen, SIZE_T), ier)
  if(ier /= 0) error stop "h5fortran:h5tset_size:character: " // dname // ' in ' // self%filename
  dtype_id = type_id
else
  type_id = dtype
endif

call H5Acreate_by_name_f(self%file_id, dname, attr, type_id, filespace_id, attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Acreate_by_name failed: " // dname // ":" // attr // " in " // self%filename

call H5Sclose_f(filespace_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:attr_create:h5sclose: space close " // dname // ":" // attr // " in " // self%filename

end subroutine attr_create


end submodule attr_smod
