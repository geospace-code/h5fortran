# h5fortran API

This document provides a listing of h5fortran `public` scoped user-facing procedures and methods with a summary of their parameters.

Reading or writing {real64,real32,int32,int64} from scalar to 7d is supported.

All examples assume:

```fortran
use h5fortran, only: hdf5_file
use hdf5, only: HSIZE_T, HID_T

type(hdf5_file) :: h
```

Query HDF5 library version:

```fortran
use h5fortran, only : hdf5version
print *, hdf5version()
```

## Character variables

Character variables are also supported for datasets and attributes.
On some compilers (e.g. oneAPI), it's more stable to use an auxiliary variable when writing character data to avoid "junk" in the string.

For example:

```fortran
character(5) :: aux

aux = "hello"

h%write("/mystr", aux)
```

rather than simply

```fortran
h%write("/mystr", "hello")
```

This is ESPECIALLY true if writing empty character to a dataset or attribute:

```fortran

h%write("/mystr", "")  !< DON'T: may have junk characters in file


!! instead do like:
character(123) :: aux  !< arbitrary length
aux = ""
h%write("/mystr", aux)
```

## Open HDF5 file reference

More than one HDF5 file can be open in a program, by declaring unique file handle (variable) like:

```fortran
type(hdf5_file) :: h1, h2, h3
```

```fortran
call h%open(filename, action, comp_lvl)
!! Opens hdf5 file

character(*), intent(in) :: filename
character(*), intent(in), optional :: action  !< 'r', 'r+', 'w', 'rw'  (default 'r')
integer, intent(in), optional      :: comp_lvl  !< 0: no compression. 1-9: ZLIB compression, higher is more compressior
```

## Close HDF5 file reference

```fortran
call h%close(close_hdf5_interface)
!! This must be called on each open file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

logical, intent(in), optional :: close_hdf5_interface
```

To avoid memory leaks or corrupted files, always "close" files before Stopping the Fortran program.

## Flush data to disk while file is open

```fortran
call h%flush()
```

## Disk variable (dataset) inquiry

To allocate variables before reading data, inquire about dataset characteristics with these procedures.

```fortran
rank = h%ndim(dataset_name)

character(*), intent(in) :: dataset_name
```

Get disk dataset shape (1D vector)

```fortran
call h%shape(dataset_name, dims)

character(*), intent(in) :: dataset_name
integer(HSIZE_T), intent(out), allocatable :: dims(:)
```

Get attribute shape (1D vector)

```fortran
call h%shape(dataset_name, dims, attr_name)

character(*), intent(in) :: dataset_name, attr_name
integer(HSIZE_T), intent(out), allocatable :: dims(:)
```

Dataset "dname" data class (i.e. integer, float, string, ...)

```fortran
integer :: class
!! H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F
class = h%class(dname)
character(*), intent(in) :: dname
```

Dataset "dname" datatype

```fortran
integer(HID_T) :: dtype
!! H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE
dtype = h%dtype(dname)
character(*), intent(in) :: dname
```

Does dataset "dname" exist in this HDF5 file?

```fortran
tf = h%exist(dname)

character(*), intent(in) :: dname
```

Does attribute "attr" exist for object "obj" in this file?

```fortran
tf = h%exist_attr(obj, attr)

character(*), intent(in) :: obj, attr
```

Is dataset "dname" contiguous on disk?

```fortran
tf = h%is_contig(dname)

character(*), intent(in) :: dname
```

Is dataset compact (< 64K)

```fortran
tf = h%is_compact(dname)
!! is dataset compact layout
character(*), intent(in) :: dname
```

Is dataset chunked?

```fortran
tf = h%is_chunked(dname)
!! is dataset chunked
character(*), intent(in) :: dname
```

Is this an HDF5 file?

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
integer, intent(out) :: chunk_size(:)
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
call h%write(dname, A, chunk_size, istart, iend, stride, compact)
!! write 0d..7d dataset
character(*), intent(in) :: dname
class(*), intent(in) :: A(:)  !< array to write
integer, intent(in), optional :: chunk_size(rank(A))
integer, intent(in), optional, dimension(:) :: istart, iend, stride  !< array slicing
logical, intent(in), optional :: compact  !< faster I/O for sub-64 kB datasets
```

Write dataset attribute (e.g. units or instrument)

```fortran
call h%writeattr(dname, attr, attrval)

character(*), intent(in) :: dname, attr  !< dataset name, attribute name
class(*), intent(in) :: attrval(:)  !< character, real, integer
```

Create group and write attributes

```fortran
call h % create_group("/elephants")
call h % write_attr("/elephants", "regions", ["Asia", "Africa"])
```

## file read operations

Read data from disk to memory

```fortran
call h%read(dname, A, istart, iend, stride)
character(*), intent(in)         :: dname
class(*), intent(out) :: A(:)  !< read array to this ALLOCATED variable
integer, intent(in), optional, dimension(:) :: istart, iend, stride !< array slicing
```

Read dataset attribute into memory

```fortran
call h%readattr(dname, attr, attrval)
character(*), intent(in) :: dname, attr  !< dataset name, attribute name
class(*), intent(out) :: attrval(:)  !< character, real, integer
```

## delete attribute

```fortran
call h%delete_attr(dname, attr)
```

## high level operations

These are single-call operations that are slower than the object-oriented methods above.
The runtime penalty may be insignificant unless you call these functions many times, say in a for loop.

The `h5write` opens `filename` with `action='rw'` (create if not present, append if existing).

```fortran
call h5write(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(in) :: A(:)
```

The `h5read` opens `filename` with `action='r'` (error if file not exist).

```fortran
call h5read(filename, dname, A)
character(*), intent(in) :: filename, dname
class(*), intent(out) :: A(:)
```
