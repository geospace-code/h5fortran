program test_deflate
!! unit tests and registration tests of HDF5 deflate compression write
!! these tests are adapted from non-MPI h5fortran.
!! several of them don't actually need MPI, but demonstrate that properties
!! can be read by each MPI worker when the file is opened wiht h5%open(..., mpi=.true.)

use, intrinsic:: iso_fortran_env, only: int32, int64, real32, real64, stderr=>error_unit

use h5fortran, only: hdf5_file

implicit none

character(*), parameter :: fn1='deflate1.h5', fn2='deflate2.h5', fn3='deflate3.h5'
integer, parameter :: N(2) = [50, 1000], &
MIN_COMP = 2  !< lots of CPUs, smaller arrays => poorer compression


call test_write_deflate(fn1, N)
print *,'OK: HDF5 write deflate'

call test_deflate_whole(fn2, N)
print *,'OK: HDF5 compress whole'

call test_deflate_slice(fn3, N)
print *,'OK: HDF5 compress slice'

contains

subroutine test_write_deflate(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N(2)

type(hdf5_file) :: h5f
real(real32), allocatable :: A(:,:)

allocate(A(N(1), N(2)))

A = 0  !< simplest data

call h5f%open(fn, action='w', comp_lvl=1)
call h5f%write('/A', A, dset_dims=N, chunk_size=[5, 50])
call h5f%close()

deallocate(A)

allocate(A(N(1), N(2)))
A = 1  !< simplest data

!! write with compression
call h5f%open(fn, action='a', comp_lvl=1)

call h5f%write('/small_contig', A(:4,:4))
!! not compressed because too small

call h5f%write('/noMPI', A)
!! write without MPI, with compression

call h5f%close()

end subroutine test_write_deflate


subroutine test_deflate_whole(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N(2)

type(hdf5_file) :: h5f
real, allocatable :: A(:,:,:)
integer :: chunks(3)
real :: fsize, crat

allocate(A(N(1), N(2), 4))

call h5f%open(fn, action='w', comp_lvl=3)

call h5f%write('/A', A, dset_dims=[N(1), N(2), 4], chunk_size=[4, 20, 1])
call h5f%chunks('/A', chunks)
if(chunks(1) /= 4 .or. chunks(3) /= 1)  then
  write(stderr, '(a,3I5)') "expected chunks: 4,*,1 but got chunks ", chunks
  error stop '#2 manual chunk unexpected chunk size'
endif

call h5f%write('/A_autochunk', A, [N(1), N(2), 4])
call h5f%chunks('/A_autochunk', chunks)
if(any(chunks < 1)) error stop '#2 auto chunk unexpected chunk size'

fsize = real(h5f%filesize())
crat = (2 * N(1) * N(2) * 4 * storage_size(A) / 8) / fsize
!! 2* since two datasets same size

print '(A,F6.2,A,f7.1)','#2 filesize (Mbytes): ', fsize / 1e6, '   compression ratio:', crat

if(crat < MIN_COMP) error stop fn // ' low compression'

call h5f%close()

end subroutine test_deflate_whole


subroutine test_deflate_slice(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N(2)

type(hdf5_file) :: h5f
integer, allocatable :: A(:,:,:)
integer :: chunks(3)
real :: fsize, crat
integer :: M(3)

M = [N(1), N(2) - 20, 4]

allocate(A(N(1)+1, M(2), 4))  !< dim 1 is deliberately not the "right" size, we will index at %write()

A = 0

call h5f%open(fn, action='w', comp_lvl=1)
print *, "#3 file opened"
call h5f%write('/A', A(:M(1), :, :), dset_dims=M)
print *, "#3 written slice of A"
call h5f%chunks('/A', chunks)
if(any(chunks < 1)) error stop '#3 auto chunk unexpected chunk size'

fsize = real(h5f%filesize())
crat = (N(1) * N(2) * storage_size(A) / 8) / fsize

print '(A,F6.2,A,f7.1)','#3 filesize (Mbytes): ',fsize / 1e6, '  compression ratio:', crat

if(crat < MIN_COMP) error stop fn // ' low compression'


call h5f%close()

end subroutine test_deflate_slice


end program
