program test_fortran_mpi

use, intrinsic :: iso_fortran_env, only : real32
use hdf5
use mpi

implicit none

integer :: ierr, mpi_id
integer(HID_T) :: fapl, dcpl, xfer_id, fid, dset_id, filespace
integer(HSIZE_T) :: chunk_size(2) = [10, 10]

real(real32), allocatable :: A(:,:)

call mpi_init(ierr)
if (ierr/=0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)

call h5open_f(ierr)
if (ierr/=0) error stop "h5open"

call h5pcreate_f(H5P_FILE_ACCESS_F, fapl, ierr)
call h5pset_fapl_mpio_f(fapl, MPI_COMM_WORLD, MPI_INFO_NULL, ierr)
if (ierr/=0) error stop "h5pset_fapl_mpio"

call h5fcreate_f('junk.h5', H5F_ACC_TRUNC_F, fid, ierr, access_prp=fapl)
if (ierr/=0) error stop "h5fcreate"
call h5pclose_f(fapl, ierr)

call h5pcreate_f(H5P_DATASET_XFER_F, xfer_id, ierr)
call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_COLLECTIVE_F, ierr)
if (ierr/=0) error stop "h5pset_dxpl_mpio"

if (mpi_id == 0) then
  allocate(A(100, 100))

  call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, ierr)
  if (ierr/=0) error stop "h5pcreate"

  call h5pset_chunk_f(dcpl, rank(A), chunk_size, ierr)
  if (ierr/=0) error stop "h5pset_chunk"

  call h5pset_deflate_f(dcpl, 1, ierr)
  if (ierr/=0) error stop "h5pset_deflate"
else
  call h5sselect_none_f(filespace, ierr)
endif

call h5screate_simple_f(rank(A), shape(A, HSIZE_T), space_id=filespace, hdferr=ierr)
if (ierr/=0) error stop "h5screate_simple"

!> create dataset
call h5dcreate_f(fid, "/test", H5T_NATIVE_REAL, space_id=filespace, dset_id=dset_id, hdferr=ierr, dcpl_id=dcpl)
if (ierr/=0) error stop "h5dcreate"

call h5dwrite_f(dset_id, H5T_NATIVE_REAL, &
A, shape(A, HSIZE_T), ierr, &
file_space_id = filespace, xfer_prp = xfer_id)
if (ierr/=0) error stop "h5dwrite"

call h5dclose_f(dset_id,ierr)
call h5pclose_f(xfer_id, ierr)
call h5pclose_f(dcpl, ierr)
call h5sclose_f(filespace, ierr)
call h5fclose_f(fid, ierr)
if (ierr/=0) error stop "h5fclose"

call h5close_f(ierr)

call mpi_finalize(ierr)

end program
