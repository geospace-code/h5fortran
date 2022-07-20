!! Example of compact vs. contiguous datasets, requires HDF5 >= 1.8
!! for convenience of comparison we create two files,
!! though distinct dataset types can exist in the same file.
!!
!! The file size saving isn't so much, but the advantage is in speed of access
!! particularly when small datasets are frequently read/write

module h5layout

use, intrinsic :: iso_fortran_env, only : REAL64,INT64, stderr=>error_unit
use hdf5
use h5lt, only : h5ltread_dataset_float_f

implicit none (type, external)

private
public :: print_layout, write_layout, sysclock2ms, std_unlink, bench_write, bench_read

contains

subroutine print_layout(filename, ds_name)
!! prints dataset (variable) layout in a file
character(*), intent(in) :: filename, ds_name

integer :: ierr, layout
integer(hid_t) :: f_id, ds_id, prp_id
INTEGER(HSIZE_T) :: fsize

CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, f_id, ierr)

!> get file size.
CALL h5fget_filesize_f(f_id, fsize, ierr)
print *, filename," size in bytes: ", fsize

!> get dataset storage layout
CALL h5dopen_f(f_id, ds_name, ds_id, ierr)
if (ierr /= 0 ) error stop "problem opening dataset"
CALL h5dget_create_plist_f(ds_id, prp_id, ierr)
CALL h5pget_layout_f(prp_id, layout, ierr)

!> not truly constants, so select case doesn't work
if (layout == H5D_COMPACT_F) then
  print *, ds_name, " layout: COMPACT"
elseif(layout == H5D_CONTIGUOUS_F) then
  print *, ds_name, " layout: CONTIGUOUS"
elseif(layout == H5D_CHUNKED_F) then
  print *, ds_name, " layout: CHUNKED"
else
  error stop "could not determine layout"
end if

call h5pclose_f(prp_id, ierr)
call h5dclose_f(ds_id, ierr)
call h5fclose_f(f_id, ierr)
CALL h5close_f(ierr)

end subroutine print_layout


subroutine write_layout(file_id, ds_name, layout, dat)
!! write real data with HDF5 layout type specified

integer(hid_t), intent(in) :: file_id
character(*), intent(in) :: ds_name
integer, intent(in) :: layout
real, intent(in) :: dat(:)

integer :: ierr
integer(hid_t) :: ds_id, prp_id, space_id
integer(HSIZE_T) :: dims(rank(dat))

dims = shape(dat, HSIZE_T)

!> create dataspace
call h5screate_simple_f(size(dims), dims, space_id, ierr)
if (ierr /= 0) error stop "problem creating dataspace"

!> create dataset property list
call h5pcreate_f(H5P_DATASET_CREATE_F, prp_id, ierr)
if (ierr /= 0) error stop "problem creating property list"

!> config for dataset
call h5pset_layout_f(prp_id, layout, ierr)
if (ierr /= 0) error stop "problem creating layout"

if(layout == H5D_CHUNKED_F) then
   !! normally chunk size set more effectively for actual large datasets.
   call h5pset_chunk_f(prp_id, size(dims), shape(dat, hsize_t), ierr)
   if (ierr /= 0) error stop "problem setting chunk size"
endif

!> create dataset
call h5dcreate_f(file_id, ds_name, H5T_NATIVE_REAL, space_id, ds_id, ierr, dcpl_id=prp_id)
if (ierr /= 0) error stop "problem creating dataset"

!> write data
CALL h5dwrite_f(ds_id, H5T_NATIVE_REAL, dat, dims, ierr)

!> close
call h5pclose_f(prp_id, ierr)
call h5dclose_f(ds_id, ierr)
call h5sclose_f(space_id, ierr)

end subroutine write_layout


impure elemental real(real64) function sysclock2ms(t)
!! Convert a number of clock ticks, as returned by system_clock() called
!! with integer(int64) arguments, to milliseconds

integer(int64), intent(in) :: t
integer(int64) :: rate
call system_clock(count_rate=rate)

sysclock2ms = t * 1000._real64 / rate

end function sysclock2ms


subroutine std_unlink(file)
character(*), intent(in) :: file
integer :: u
open(newunit=u, file=file)
close(u, status="delete")
end subroutine std_unlink


subroutine bench_write(file, layout, dat, N)
character(*), intent(in) :: file
integer, intent(in) :: layout, N
real, intent(in) :: dat(:)

integer(hid_t) :: file_id
integer(int64) :: tic, toc
integer :: ierr, i
character(6) :: name

CALL h5fcreate_f(file, H5F_ACC_TRUNC_F, file_id, ierr)
if (ierr /= 0) error stop "problem creating " // file

call system_clock(count=tic)
do i = 1,N
  write (name, '(A2,I0.4)') '/x',i
  call write_layout(file_id, name, layout, dat)
end do
call system_clock(count=toc)
print *, file,N,"variables: elapsed millisec:",sysclock2ms(toc-tic)

CALL h5fclose_f(file_id, ierr)

end subroutine bench_write


subroutine bench_read(file, N, dat)

character(*), intent(in) :: file
integer, intent(in) :: N
real, intent(out), optional :: dat(:)

integer(hid_t) :: file_id
integer(int64) :: tic, toc
integer(HSIZE_T) :: dims(rank(dat))
integer :: ierr, i
character(6) :: name
real, allocatable :: dbuf(:)

allocate(dbuf(N))
dims = shape(dbuf, HSIZE_T)

CALL h5fopen_f(file, H5F_ACC_RDONLY_F, file_id, ierr)
if (ierr /= 0) error stop "problem opening " // file

call system_clock(count=tic)
do i = 1,N
  write (name, '(A2,I0.4)') '/x',i
  call h5ltread_dataset_float_f(file_id, name, dbuf, dims, ierr)
end do
call system_clock(count=toc)
print *, file,N,"variables: elapsed millisec:",sysclock2ms(toc-tic)

CALL h5fclose_f(file_id, ierr)

end subroutine bench_read


end module h5layout


program compact

use hdf5
use h5layout, only : bench_read, bench_write, std_unlink

implicit none (type, external)

integer :: N, M, ierr

character(:), allocatable :: fn1, fn2, fn3

real, allocatable :: x(:) !< dummy data, could be any TKR

N = 1000
M = 100

fn1 = "bench_compact.h5"
fn2 = "bench_contiguous.h5"
fn3 = "bench_chunked.h5"

!> dummy data
allocate(x(M))

!> Fortran interface
CALL h5open_f(ierr)

!------- compact
call bench_write(fn1, H5D_COMPACT_F, x, N)
call bench_read(fn1, N)

!------- contiguous
call bench_write(fn2, H5D_CONTIGUOUS_F, x, N)
call bench_read(fn2, N)

!-------- chunked
call bench_write(fn3, H5D_CHUNKED_F, x, N)
call bench_read(fn3, N)

!---- cleanup
call h5close_f(ierr)

end program
