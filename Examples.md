# h5fortran Examples

All examples assume:

```fortran
use h5fortran, only: hdf5_file
type(hdf5_file) :: h5f
```

* gzip compression may be applied for rank &ge; 2 arrays by setting `comp_lvl` to a value between 1 and 9.
  Shuffle filter is automatically applied for better compression
* string attributes may be applied to any variable at time of writing or later.
* h5f%initialize(..., `comp_lvl=1`) option enables GZIP compression., where comp_lvl is from 1 to 9. bigger comp_lvl gives more compression but isslower to write.

`integer, intent(out) :: ierr` is an optional parameter. It will be non-zero if error detected.
This value should be checked, particularly for write operations to avoid missing error conditions.
If `ierr` is omitted, then h5fortran will raise `error stop` if an error occurs.

## Create new HDF5 file, with variable "value1"

```fortran
call h5f%initialize('test.h5', status='new',action='w')

call h5f%write('/value1', 123.)

call h5f%finalize()
```

## ensure all files are flushed to disk at end of program

If your program opens lots of HDF5 files and you're worried about being sure they're all flushed to disk, make this call near the very end of the program.
This flushes and closes ALL HDF5 files, even those that may be invoked directly from the HDF5 library without h5fortran.

```fortran
call hdf5_close()
```

Normally, you should be calling `%finalize()` on each file to flush to disk when done using a file.
If `%finalize()` or hdf5_close is not called, data loss can result.

```fortran
call h5f%finalize()
```

At any time during the program, the `%flush()` method can be called to request the operating system to write a file to disk.
This could be useful during a long-running program (say, an HPC simulation) to help ensure data isn't lost of an HDF5 file is open for a long time.
The flush request is on a per-file basis, so if multiple files are open, flush each file to protect against data loss in this case.

```fortran
call h5f%flush()
```

## create temporary "scratch" file

Analogous to regular Fortran `open(status='scratch')`, the file created will attempt to be deleted.
A distinction is that filename (or full path) must be specified.
If the full path is not specified, the system temporary directory will be used if found.
Otherwise, the current working directory + filename will be used.

```sh
call h5%initialize('orbits.h5', status='scratch')

...

call h5%finalize()
!! scratch file deleted by %finalize
```

## Add/append variable "value1" to existing HDF5 file "test.h5"

* if file `test.h5` exists, add a variable to it
* if file `test.h5` does not exist, create it and add a variable to it.

```fortran
call h5f%initialize('test.h5', status='unknown',action='rw')

call h5f%write('/value1', 123.)

call h5f%finalize(ierr)
```

## Add gzip compressed 3-D array "value2" to existing HDF5 file "test.h5"

```fortran
real :: val2(1000,1000,3) = 0.

call h5f%initialize('test.h5', comp_lvl=1)

call h5f%write('/value2', val2)

call h5f%finalize(ierr)
```

chunk_size may optionally be set in the `%write()` method for 2-d to 7-d arrays.
compression and chunking are disabled if any element of chunk_size is less than 1
chunk_size may be manually specified in write() otherwise it will be set automatically.

Currently, data is written contiguous if not compressed and is only chunked if compression is used.

## check if a variable exists

the logical method %exist() checks if a dataset (variable) exists in the initialized HDF5 file.

```fortran
exists = h5f%exist("/foo")
```

A convenience method that checks existance of a dataset without creating the h5 object manually is:

```fortran
exists = h5exist("my.h5", "/foo")
```

## check variable shape, rank/ndims

`h5f%ndims` we didn't use `%rank` to avoid confusion with intrinsic "rank()"

```fortran
call h5f%initialize('test.h5', status='old',action='r')

integer :: drank
integer(hsize_t), allocatable :: dims(:)

drank = h5f%ndims('/foo')
call h5f%shape('/foo',dims)

if (drank /= size(dims)) error stop
```


## Read scalar, 3-D array of unknown size

```fortran
call h5f%initialize('test.h5', status='old',action='r')

integer(hsize_t), allocatable :: dims(:)
real, allocatable :: A(:,:,:)

call h5f%shape('/foo',dims)
allocate(A(dims(1), dims(2), dims(3)))
call h5f%read('/foo', A)

call h5f%finalize()
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

call h5f%initialize('test.h5', status='old',action='r')

call h5f%read('/foo', A, istart=[5, 1, 2], iend=[7, 5, 8])
```

## is dataset contiguous or chunked

Assumed file handle h5f was already initialized, the logical status is inspected:

```fortran
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

call h5f%initialize('test.h5')

call h5f%write_group('/scope/')

call h5f%finalize()
```

## verbose / debug

set options debug and /or verbose for diagnostics

```sh
call h5f%initialize(..., verbose=.true., debug=.true.)
```

## Permissive syntax

We make the hdf5%open(..., status=...) like Fortran open()

* overwrite (truncate) existing file: open with `status='new'` or `status='replace'`
* append to existing file or create file: `status='old'` or `status='unknown'`
