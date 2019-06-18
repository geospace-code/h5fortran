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
!! FIXME: need to use another function to read length, as simply reads gibberish after actual data
!! till buffer is full

integer :: ierr
!integer(HSIZE_T), allocatable :: dims(:)

!> This is a random integer, how to retrieve length of string?
!call hdf_get_shape(self, dname, dims)
!print *, dname, dims(1)

call h5ltread_dataset_string_f(self%lid, dname, value, ierr)
if (ierr /= 0) error stop 'error on dataset ' //dname// 'read ' //self%filename

end procedure hdf_get_string


end submodule read
