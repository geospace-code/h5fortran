submodule (h5fortran) read
!! This submodule is for reading HDF5 via submodules
use hdf5, only : h5dopen_f, h5dread_f, h5dclose_f, h5dget_create_plist_f, &
  h5pget_layout_f, H5D_CONTIGUOUS_F, H5D_CHUNKED_F
use H5LT, only : h5ltread_dataset_f, h5ltread_dataset_int_f, h5ltread_dataset_float_f, h5ltread_dataset_double_f,&
  h5ltread_dataset_string_f, h5ltpath_valid_f

implicit none

contains


module procedure hdf_get_shape
!! must get dims before info, as "dims" must be allocated or segfault occurs.
integer(SIZE_T) :: dsize
integer :: dtype, drank

if (.not.self%exist(dname)) then
  write(stderr, *) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5ltget_dataset_ndims_f(self%lid, dname, drank, ierr)
if (check(ierr, 'ERROR: '// dname // ' rank ' // self%filename)) return

allocate(dims(drank))
call h5ltget_dataset_info_f(self%lid, dname, dims, dtype, dsize, ierr)
if (check(ierr, 'ERROR: ' // dname // ' info ' // self%filename)) return

end procedure hdf_get_shape


module procedure hdf_get_layout

integer(HID_T) :: pid, did
integer :: ierr

layout = -1

if (.not.self%exist(dname)) then
  write(stderr, *) 'ERROR: ' // dname // ' does not exist in ' // self%filename
  ierr = -1
  return
endif

call h5dopen_f(self%lid, dname, did, ierr)
if (check(ierr, 'ERROR: open dataset ' // dname // ' read layout ' // self%filename)) return
call h5dget_create_plist_f(did, pid, ierr)
if (check(ierr, 'ERROR: get property list ID ' // dname // ' read layout ' // self%filename)) return
call h5pget_layout_f(pid, layout, ierr)
if (check(ierr, 'ERROR: get_layout ' // dname // ' read layout ' // self%filename)) return
call h5dclose_f(did, ierr)
if (check(ierr, 'ERROR: close dataset: ' // self%filename)) return

end procedure hdf_get_layout


module procedure hdf_is_contig
hdf_is_contig = self%layout(dname) == H5D_CONTIGUOUS_F
end procedure hdf_is_contig

module procedure hdf_is_chunked
hdf_is_chunked = self%layout(dname) == H5D_CHUNKED_F
end procedure hdf_is_chunked


module procedure hdf_check_exist

integer :: ierr

if (self%lid == 0) then
  write(stderr,*) 'ERROR: must initialize file before checking existance of variable'
  exists = .false.
  return
endif

call h5ltpath_valid_f(self%lid, &
  path=dname, &
  check_object_valid=.true., &
  path_valid=exists, &
  errcode=ierr)

if (ierr/=0) then
  write(stderr,*) 'ERROR: could not determine status of ' // dname // ' in ' // self%filename
  return
endif

end procedure hdf_check_exist

end submodule read
