# h5fortran Examples

All examples assume:

```fortran
use h5fortran, only: hdf5_file
type(hdf5_file) :: h5f
```

* gzip compression may be applied for rank &ge; 2 arrays by setting `comp_lvl` to a value between 1 and 9.
  Shuffle filter is automatically applied for better compression
* string attributes may be applied to any variable at time of writing or later.
* h5f%open(..., `comp_lvl=1`) option enables GZIP compression., where comp_lvl is from 1 to 9. bigger comp_lvl gives more compression but isslower to write.

`integer, intent(out) :: ierr` is an optional parameter. It will be non-zero if error detected.
This value should be checked, particularly for write operations to avoid missing error conditions.
If `ierr` is omitted, then h5fortran will raise `error stop` if an error occurs.

## Create new HDF5 file, with variable "value1"

```fortran
call h5f%open('test.h5', status='new')

call h5f%write('/value1', 123.)

call h5f%close()
```

## create soft links to actual variable

HDF5 soft link variables: arbitrarily many soft-linked variable names can point to an actual variable, which need not yet exist.

```fortran
call h5f%write("/x", 42)
call h5f%softlink("/x", "/y")
call h5f%softlink("/x", "/z")
```

/z and /y are soft links to /x, which need not yet exist.

## ensure all files are flushed to disk at end of program

If your program opens lots of HDF5 files and you're worried about being sure they're all flushed to disk, make this call near the very end of the program.
This flushes and closes ALL HDF5 files, even those that may be invoked directly from the HDF5 library without h5fortran.

```fortran
call hdf5_close()
```

Normally, you should be calling `%close()` on each file to flush to disk when done using a file.
If `%close()` or hdf5_close is not called, data loss can result.

```fortran
call h5f%close()
```

At any time during the program, the `%flush()` method can be called to request the operating system to write a file to disk.
This could be useful during a long-running program (say, an HPC simulation) to help ensure data isn't lost of an HDF5 file is open for a long time.
The flush request is on a per-file basis, so if multiple files are open, flush each file to protect against data loss in this case.

```fortran
call h5f%flush()
```

## read / write attributes to variable

Note that HDF5 character attributes are scalar while int32, real32, real64 attributes are 1D vectors.

Assume variable "/x" exists and then see these examples:

### write attributes

```fortran
call h%writeattr('/x', 'note','this is just a little number')
call h%writeattr('/x', 'hello', 'hi')
call h%writeattr('/x', 'life', [42])
call h%writeattr('/x', 'life_float', [42._real32, 84._real32])
call h%writeattr('/x', 'life_double', [42._real64])
```

or for the high level interface:

```fortran
call h5write_attr('myfile.h5', '/x', 'date', [2020,4,1])

call h5write_attr('myfile.h5', '/x', 'units', 'Nm^-2')
```

### read attributes

For attributes, HDF5 character values are *space-terminated* instead of null terminated.

```fortran
character(1024) :: attr_str
integer :: attr_int(1)
real(real32) :: attr32(2)
real(real64) :: attr64(1)

call h%readattr('/x', 'note', attr_str)
if (attr_str /= 'this is just a little number') error stop 'readattr value note'

call h%readattr('/x', 'life', attr_int)
call h%readattr('/x', 'life_float', attr32)
call h%readattr('/x', 'life_double', attr64)
```

or for the high level interface:

```fortran
integer :: idate(3)
character(16) :: unit_str

call h5read_attr('myfile.h5', '/x', 'date', idate)

call h5read_attr('myfile.h5', '/x', 'units', unit_str)
```

## create temporary "scratch" file

Analogous to regular Fortran `open(status='scratch')`, the file created will attempt to be deleted.
A distinction is that filename (or full path) must be specified.
If the full path is not specified, the system temporary directory will be used if found.
Otherwise, the current working directory + filename will be used.

```sh
call h5%open('orbits.h5', status='scratch')

...

call h5%close()
!! scratch file deleted by %close
```

## Add/append variable "value1" to existing HDF5 file "test.h5"

* if file `test.h5` exists, add a variable to it
* if file `test.h5` does not exist, create it and add a variable to it.

```fortran
call h5f%open('test.h5', status='unknown',action='rw')

call h5f%write('/value1', 123.)

call h5f%close(ierr)
```

## Add gzip compressed 3-D array "value2" to existing HDF5 file "test.h5"

```fortran
real :: val2(1000,1000,3) = 0.

call h5f%open('test.h5', comp_lvl=1)

call h5f%write('/value2', val2)

call h5f%close(ierr)
```

chunk_size may optionally be set in the `%write()` method for 2-d to 7-d arrays.
compression and chunking are disabled if any element of chunk_size is less than 1
chunk_size may be manually specified in write() otherwise it will be set automatically.

Currently, data is written contiguous or compact if not compressed and is only chunked if compression is used.

## check if a variable exists

the logical method %exist() checks if a dataset (variable) exists in the opened HDF5 file.

```fortran
exists = h5f%exist("/foo")
```

A convenience method that checks existence of a dataset without creating the h5 object manually is:

```fortran
exists = h5exist("my.h5", "/foo")
```

## check variable shape, rank/ndims

`h5f%ndims` we didn't use `%rank` to avoid confusion with intrinsic "rank()"

```fortran
call h5f%open('test.h5', status='old',action='r')

integer :: drank
integer(hsize_t), allocatable :: dims(:)

drank = h5f%ndims('/foo')
call h5f%shape('/foo',dims)

if (drank /= size(dims)) error stop
```

## Read scalar, 3-D array of unknown size

```fortran
call h5f%open('test.h5', status='old',action='r')

integer(hsize_t), allocatable :: dims(:)
real, allocatable :: A(:,:,:)

call h5f%shape('/foo',dims)
allocate(A(dims(1), dims(2), dims(3)))
call h5f%read('/foo', A)

call h5f%close()
```

## read slice (part of) a disk array

Reading a disk HDF5 array into a variable of matching shape is done with `istart=` and `iend=` arguments, which have 1-D arguments for the start and stop index desired from each dimension.

For example, support HDF5 disk variable "/foo" is shape (10,20,30) and you wish to read just part of this array like:

* dim 1: 5-7
* dim 2: 1-5
* dim 3: 2-8

then do:

```fortran
real, dimension(3,5,7) :: A

call h5f%open('test.h5', status='old',action='r')

call h5f%read('/foo', A, istart=[5, 1, 2], iend=[7, 5, 8])
```

## write slice (part of) a disk array

Writing a disk HDF5 array from a variable of matching shape is done with `istart=` and `iend=` arguments, which have 1-D arguments for the start and stop index desired from each dimension.

For example, support HDF5 disk variable "/foo" is shape (10,20,30) and you wish to write a slice from a variable shaped (5,7,1) with start/stop indices:

* dim 1: 3-7
* dim 2: 4-10
* dim 3: 8

then do:

```fortran
real, dimension(5,7,1) :: A

call h5f%open('test.h5', status='unknown')

call h5f%create('/foo', H5T_NATIVE_REAL, [5,7,1])
call h5f%write('/foo', A, istart=[3, 4, 8], iend=[7, 10, 8])
```

Note the h5f%create() call to open the disk variable.
This step is also needed with h5py in Python or Matlab HDF5 h5create() before h5write().

## is dataset compact, contiguous, or chunked

Assume file handle h5f was already opened, the logical status is inspected:

```fortran
is_compact = h5f%is_compact("/foo")

is_contig = h5f%is_contig('/foo')

is_chunked = h5f%is_chunked('/foo')
```

## get chunk size

if dataset is not chunked, chunk_size == -1

```sh
call h5f%chunks('/foo', chunk_size)
```

## Create group "scope"

```fortran
real :: val2(1000,1000,3) = 0.

call h5f%open('test.h5')

call h5f%write_group('/scope/')

call h5f%close()
```

## verbose / debug

set options debug and /or verbose for diagnostics

```sh
call h5f%open(..., verbose=.true., debug=.true.)
```

## Permissive syntax

We make the hdf5%open(..., status=...) like Fortran open()

* overwrite (truncate) existing file: open with `status='new'` or `status='replace'`
* append to existing file or create file: `status='old'` or `status='unknown'`
