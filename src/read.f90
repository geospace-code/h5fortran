!! This submodule is for reading HDF5, via child submodules
submodule (hdf5_interface) read

use H5LT, only: h5ltget_dataset_info_f, h5ltread_dataset_f, h5ltget_dataset_info_f, h5ltget_dataset_ndims_f, &
    h5dopen_f, h5dread_f, h5dclose_f, h5ltread_dataset_string_f
implicit none
contains


module procedure hdf_get_shape
!! must get dims before info, as "dims" must be allocated or segfault occurs.
integer(SIZE_T) :: dsize
integer :: ierr, dtype, drank

call h5ltget_dataset_ndims_f(self%lid, dname, drank, ierr)
if (ierr /= 0) error stop 'error opening dataset '//dname// ' rank ' //self%filename

allocate(dims(drank))
call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (ierr /= 0) error stop 'error opening dataset ' //dname// ' info ' //self%filename

end procedure hdf_get_shape


module procedure hdf_get_string
!! Need to use "buf" variable, even intent(inout) doesn't help without
!! separate "buf" variable

integer :: ierr
character(len(value)) :: buf

call h5ltread_dataset_string_f(self%lid, dname, buf, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// 'read ' //self%filename

value = buf

end procedure hdf_get_string


end submodule read
