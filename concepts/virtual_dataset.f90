program vds_simple

use hdf5

implicit none

character(*), parameter :: vfn = "demo_vds_simple.h5", src_fn="a.h5", src_name = "/A", v_name = "/V"
! character(1000) :: src_filename, dsetname

integer(HSIZE_T), parameter :: dims(2) = [4, 6] !< Source dataset dimensions

integer(HID_T) ::  fid, space, src_space, vspace, dset_id,dcpl
integer :: ier
integer(HSIZE_T) :: vdsdims(2) = dims !< Virtual dataset dimension
integer :: wdata(dims(1), dims(2)) !< Write buffer for source dataset
integer :: rdata(dims(1), dims(2)) !< Read buffer for virtual dataset
integer(SIZE_T) :: i, j
integer(SIZE_T) :: L !< Length of the string; also a return value


integer :: layout !< Storage layout
integer :: type
integer(SIZE_T) :: num_map !< Number of mappings

!> Initialize data.
do i = 1, dims(1)
  do j = 1, dims(2)
      wdata(i, j) = i + 1
  end do
end do

CALL h5open_f(ier)

!> Create the source file and the dataset. Write data to the source dataset
!> and close all resources.

call H5Fcreate_f(src_fn, H5F_ACC_TRUNC_F, fid, ier)
call H5Screate_simple_f(size(dims), dims, space, ier)
call H5Dcreate_f(fid, src_name, H5T_NATIVE_INTEGER, space, dset_id, ier)
call H5Dwrite_f(dset_id, H5T_NATIVE_INTEGER, wdata, dims, ier);
call H5Sclose_f(space, ier)
call H5Dclose_f(dset_id, ier)
call H5Fclose_f(fid, ier)

!> Create file in which virtual dataset will be stored.
call H5Fcreate_f(vfn, H5F_ACC_TRUNC_F, fid, ier)

!> Create VDS dataspace
call H5Screate_simple_f(size(dims), vdsdims, vspace, ier)

!> Set VDS creation property
call H5Pcreate_f(H5P_DATASET_CREATE_F, dcpl, ier)

!> Build the mappings.
!> Selections in the source datasets are H5S_ALL.
!> In the virtual dataset we select the first, the second and the third rows
!> and map each row to the data in the corresponding source dataset.
call H5Screate_simple_f(size(dims), dims, src_space, ier)
if(ier/=0) error stop "H5Screate_simple_f" // src_fn
call H5Pset_virtual_f(dcpl, vspace, src_fn, src_name, src_space, ier)
if (ier/=0) error stop "H5Pset_virtual_f " // src_name

!> Create a virtual dataset
call H5Dcreate_f(fid, v_name, H5T_NATIVE_INTEGER, vspace, dset_id, ier, dcpl_id=dcpl)
if(ier/=0) error stop "H5Dcreate_f " // v_name
call H5Sclose_f(vspace, ier)
call H5Sclose_f(src_space, ier)
call H5Dclose_f(dset_id, ier)
call H5Fclose_f(fid, ier)

!> READ: Open the file and virtual dataset

call H5Fopen_f(vfn, H5F_ACC_RDONLY_F, fid, ier)
call H5Dopen_f(fid, v_name, dset_id, ier)

!> Get creation property list and mapping properties.
call H5Dget_create_plist_f(dset_id, dcpl, ier)

!> Get storage layout.

call H5Pget_layout_f(dcpl, layout, ier)
if(layout == H5D_VIRTUAL_F) then
  print '(a)', 'Dataset has virtual layout'
else
  print '(a)', 'Wrong layout found'
endif

!> Find the number of mappings.

call H5Pget_virtual_count_f(dcpl, num_map, ier)
print '(A,i0)', "Number of mappings: ", num_map

!> Get mapping parameters for each mapping.

do i = 0, num_map-1
  print '(a,i0)', "Mapping ", i
  print '(a)', 'Selection in the virtual dataset'
  !> Get selection in the virttual  dataset
  call H5Pget_virtual_vspace_f(dcpl, i, vspace, ier)
  if(ier/=0) error stop "H5Pget_virtual_vspace_f"

  !> Make sure it is ALL selection and then print selection.
  call H5Sget_select_type_f(vspace, type, ier)
  if (type == H5S_SEL_ALL_F) print '(a)', "Selection is H5S_ALL"

  ! !> Get source file name.
  ! call H5Pget_virtual_filename_f(dcpl, i, src_filename, ier, L)
  ! if(ier/=0) error stop "H5Pget_virtual_filename_f"
  ! print '(a)',  "Source filename " // src_filename(:L)

  ! !> Get source dataset name
  ! call H5Pget_virtual_dsetname_f(dcpl, i, dsetname, ier, L)
  ! print '(a)', "Source dataset name " // dsetname(:L)

  !> Get selection in the source dataset.
  print '(a)', "Selection in the source dataset"
  call H5Pget_virtual_srcspace_f(dcpl, i, src_space, ier)
  if(ier/=0) error stop "H5Pget_virtual_srcspace_f"

  !> Make sure it is ALL selection and then print selection
  call H5Sget_select_type_f(src_space, type, ier)
  if (type == H5S_SEL_ALL_F) print '(a)', "Selection is H5S_ALL"

  call H5Sclose_f(vspace, ier)
  call H5Sclose_f(src_space, ier)

end do

!> Read the data using the default properties.

call H5Dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, dims, ier)

print '(a)', "VDS Data:"
do i = 1, dims(1)
    print '(10i0)', rdata(i,:)
end do

if (any(rdata /= wdata)) error stop "data in VDS read didn't match known original data"

call H5Pclose_f(dcpl, ier)
call H5Dclose_f(dset_id, ier)
call H5Fclose_f(fid, ier)

CALL h5close_f(ier)

end program
