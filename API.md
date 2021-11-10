# h5fortran API

This document provides a listing of h5fortran `public` scoped user-facing procedures and methods with a summary of their parameters.

All examples assume:

```fortran
use h5fortran, only: hdf5_file
use hdf5, only: HSIZE_T, HID_T

type(hdf5_file) :: h
```

## Open / close HDF5 file reference

More than one HDF5 file can be open in a program, by declaring unique file handle (variable) like:

```fortran
type(hdf5_file) :: h1, h2, h3
```

```fortran
call h%open(filename,ierr, action,comp_lvl,verbose,debug)
!! Opens hdf5 file

character(*), intent(in) :: filename
integer, intent(out), optional :: ierr  !< 0 if OK
character(*), intent(in), optional :: action  !< r, w, rw
integer, intent(in), optional      :: comp_lvl  !< 0: no compression. 1-9: ZLIB compression, higher is more compressior
logical, intent(in), optional      :: verbose, debug
```

```fortran
call h%close(ierr, close_hdf5_interface)
!! This must be called on each HDF5 file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! We don't reference count because applications might also invoke HDF5
!! directly.
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

integer, intent(out), optional :: ierr
logical, intent(in), optional :: close_hdf5_interface
```

To avoid memory leaks or corrupted files, always "finalize" all hDF5 files before STOPping the Fortran program.

```fortran
call h%flush(ierr)
!! request operating system flush data to disk.
!! The operating system can do this when it desires, which might be a while.
integer, intent(out), optional :: ierr
```

## Disk variable (dataset) inquiry

To allocate variables before reading data, inquire about dataset characteristics with these procedures.

```fortran
rank = h%ndims(dataset_name)

character(*), intent(in) :: dataset_name
```

```fortran
call h%shape(dataset_name, dims, ierr)
character(*), intent(in) :: dataset_name
integer(HSIZE_T), intent(out), allocatable :: dims(:)
integer, intent(out), optional :: ierr
```

```fortran
exists = h%exist(dname)
!! does dataset "dname" exist in this file
character(*), intent(in) :: dname
```

```fortran
tf = h%is_contig(dname)
!! is dataset contiguous
character(*), intent(in) :: dname
```

```fortran
tf = h%is_compact(dname)
!! is dataset compact layout
character(*), intent(in) :: dname
```

```fortran
tf = h%is_chunked(dname)
!! is dataset chunked
character(*), intent(in) :: dname
```

```fortran
use h5fortran, only: is_hdf5

tf = is_hdf5('myfile.txt')  !< probably false
tf = is_hdf5('myfile.h5')  !< true if a valid HDF5 file
```

These are more advanced inquiries into the memory layout of the dataset, for advanced users:

```fortran
Layout = h%layout(dname)
!! integer :: H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_VIRTUAL_F, H5D_COMPACT_F
character(*), intent(in) :: dname
```

```fortran
call h%chunks(dname, chunk_size)
character(*), intent(in) :: dname
integer(hsize_t), intent(out) :: chunk_size(:)
```

## create dataset softlink

One of the key features of HDF5 is the ability to create dataset softlinks within an HDF5 file:

```fortran
call h%softlink(target, link)
character(*), intent(in) :: target, &  !< target path to link dataset
                            link  !< soft link path to create
```

## file write operations

```fortran
call h%write(dname,value, ierr, chunk_size, istart, iend, stride, compact)
!! write 0d..7d dataset
character(*), intent(in) :: dname
class(*), intent(in) :: value(:)  !< array to write
integer, intent(in), optional :: chunk_size(1)
integer, intent(in), optional, dimension(:) :: istart, iend, stride  !< array slicing
logical, intent(in), optional :: compact  !< faster I/O for sub-64 kB datasets
integer, intent(out), optional :: ierr  !< 0 if OK
```

While the generic `%write()` method above works for all supported types and ranks, it's also possible to specify the desired type to write.
The user may desire this to be more explicit about the data type intended for disk writes.
The optional arguments are the same as above; we give basic examples here:

```fortran
call h%write_r32(dname, 1._real32)

call h%write_r64(dname, 1._real64)

call h%write_i32(dname, 1_int32)

call h%write_i64(dname, 1_int64)

call h%write_char(dname, "hello")
```

Write dataset attribute (e.g. units or instrument)

```fortran
call h%writeattr(dname, attr, attrval, ierr)
character(*), intent(in) :: dname, attr  !< dataset name, attribute name
class(*), intent(in) :: attrval(:)  !< character, real, integer
```

## file read operations

Read data from disk to memory

```fortran
call h%read(dname, value, ierr, istart, iend, stride)
character(*), intent(in)         :: dname
class(*), intent(out) :: value(:)  !< read array to this ALLOCATED variable
integer, intent(out), optional :: ierr  !< 0 if OK
integer, intent(in), optional, dimension(:) :: istart, iend, stride !< array slicing
```

Read dataset attribute into memory

```fortran
call h%readattr(dname, attr, attrval, ierr)
character(*), intent(in) :: dname, attr  !< dataset name, attribute name
class(*), intent(out) :: attrval(:)  !< character, real, integer
```

## high level operations

These are single-call operations that are slower than the object-oriented methods above.
The runtime penalty may be insignificant unless you call these functions many times, say in a for loop.

The `h5write` opens `filename` with `action='rw'` (create if not present, append if existing).

```fortran
call h5write(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: value(:)
```

The `h5read` opens `filename` with `action='r'` (error if file not exist).

```fortran
call h5read(filename, dname, value)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: value(:)
```
