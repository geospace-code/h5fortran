# Object-oriented Fortran 2008 HDF5 interface

[![DOI](https://zenodo.org/badge/128736984.svg)](https://zenodo.org/badge/latestdoi/128736984)

![ci_linux](https://github.com/scivision/h5fortran/workflows/ci_linux/badge.svg)
![ci_mac](https://github.com/scivision/h5fortran/workflows/ci_mac/badge.svg)

Simple, robust, thin HDF5 polymorphic read/write interface.
Reading or writing {real64,real32,int32} from scalar to 7d is as simple as

```fortran
use h5fortran

call h5write('foo.h5', '/x', x)

call h5read('bar.h5', '/y', y)
```

* For NetCDF4 see [nc4fortran](https://github.com/scivision/nc4fortran/).
* Designed for easy use as a Meson "subproject" or CMake "ExternalProject / FetchContent" using **static** or **shared** linking.

Uses Fortran 2008 `submodule` for clean template structure.
This easy-to-use, thin object-oriented modern Fortran library abstracts away the messy parts of HDF5 so that you can read/write various types/ranks of data with a single command.
In distinction from other high-level HDF5 interfaces, h5fortran works to deduplicate code, using polymorphism wherever feasible and extensive test suite.

Polymorphic API with read/write for types int32, real32, real64 with rank:

* scalar (0-D)
* 1-D .. 7-D

as well as character (string) variables and attributes.
Mismatched datatypes are coerced as per standard Fortran rules.
For example, reading a float HDF5 variable into an integer Fortran variable:  42.3 => 42

Tested on systems with HDF5 1.8, 1.10 and 1.12 including:

* MacOS (homebrew)
* Ubuntu 16.04 / 18.04 (gfortran 6 or newer)
* Windows Subsystem for Linux
* Windows MSYS2

Currently, Cygwin does not have *Fortran* HDF5 libraries.

## Build

Requirements:

* modern Fortran compiler (this project uses `submodule` and `error stop`). For example, Gfortran &ge; 6.
* HDF5 Fortran library (>= 1.8.7, including 1.10.x and 1.12.x)
  * Mac / Homebrew: `brew install gcc hdf5`
  * Linux: `apt install gfortran libhdf5-dev`
  * Windows Subsystem for Linux: `apt install gfortran libhdf5-dev`
  * Windows MSYS2: `pacman -S mingw-w64-x86_64-hdf5`

Note that some precompiled HDF5 libraries include C / C++ without Fortran.
Platforms that currently do **not** have Fortran HDF5 libraries, and thus will **not** work with h5fortran unless you compile HDF5 library for Fortran include:

* Cygwin
* Conda

Build this HDF5 OO Fortran interface with Meson or CMake.
The library `libh5fortran` is built, link it into your program as usual.

### Meson

To build h5fortran as a standalone project

```sh
meson build

meson test -C build
```

Meson &ge; 0.53.0 has enhanced HDF5 dependency finding and is recommended.
To include h5fortran as a Meson subproject, in the master project meson.build (that uses h5fortran) have like:

```meson
hdf5_proj = subproject('h5fortran')
hdf5_interface = hdf5_proj.get_variable('hdf5_interface')

my_exe = executable('myexe', 'main.f90', dependencies: hdf5_interface)
```

and have a file in the master project `subprojects/h5fortran.wrap` containing:

```ini
[wrap-git]
directory = h5fortran
url = https://github.com/scivision/h5fortran.git
revision = head
```

### CMake

```sh
cmake -B build

cmake --build build --parallel
```

Optionally run self-tests:

```sh
cd build

ctest -V
```

To specify a particular HDF5 library, use

```sh
cmake -DHDF5_ROOT=/path/to/hdf5lib -B build
```

or set environment variable `HDF5_ROOT=/path/to/hdf5lib`

To use CMake target `h5fortran` via CMake FetchContent:

```cmake
include(FetchContent)

FetchContent_Declare(h5fortran_proj
  GIT_REPOSITORY https://github.com/scivision/h5fortran.git
  GIT_TAG master  # whatever desired version is
)

FetchContent_MakeAvailable(h5fortran_proj)

# ------------------------------------------------------
# whatever your program is
add_executable(myProj main.f90)
target_link_libraries(myProj h5fortran::h5fortran)
```

## Usage

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

### Create new HDF5 file, with variable "value1"

```fortran
call h5f%initialize('test.h5', status='new',action='w')

call h5f%write('/value1', 123.)

call h5f%finalize(ierr)
```

### Add/append variable "value1" to existing HDF5 file "test.h5"

* if file `test.h5` exists, add a variable to it
* if file `test.h5` does not exist, create it and add a variable to it.

```fortran
call h5f%initialize('test.h5', status='unknown',action='rw')

call h5f%write('/value1', 123.)

call h5f%finalize(ierr)
```

### Add gzip compressed 3-D array "value2" to existing HDF5 file "test.h5"

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

### check if a variable exists

the logical method %exists() checks if a dataset (variable) exists in the initialized HDF5 file.

```fortran
exists = h5f%exists("/foo")
```

### Read scalar, 3-D array of unknown size

```fortran
call h5f%initialize('test.h5', status='old',action='r')

integer(hsize_t), allocatable :: dims(:)
real, allocatable :: A(:,:,:)

call h5f%shape('/foo',dims)
allocate(A(dims(1), dims(2), dims(3)))
call h5f%read('/foo', A)

call h5f%finalize()
```

### is dataset contiguous or chunked

Assumed file handle h5f was already initialized, the logical status is inspected:

```fortran
is_contig = h5f%is_contig('/foo')

is_chunked = h5f%is_chunked('/foo')
```

### get chunk size

if dataset is not chunked, chunk_size == -1

```sh
call h5f%chunks('/foo', chunk_size)
```

### Create group "scope"

```fortran
real :: val2(1000,1000,3) = 0.

call h5f%initialize('test.h5')

call h5f%write_group('/scope/')

call h5f%finalize()
```

### verbose / debug

set options debug and /or verbose for diagnostics

```sh
call h5f%initialize(..., verbose=.true., debug=.true.)
```

## Permissive syntax

We make the hdf5%open(..., status=...) like Fortran open()

* overwrite (truncate) existing file: open with `status='new'` or `status='replace'`
* append to existing file or create file: `status='old'` or `status='unknown'`

## Notes

* The first character of the filename should be a character, NOT whitespace to avoid file open/creation errors.
* Using compilers like PGI or Flang may require first compiling the HDF5 library yourself.
* Intel compiler HDF5 [compile notes](https://www.hdfgroup.org/downloads/hdf5/source-code/)
* Polymorphic array rank is implemented by explicit code internally. We could have used pointers, but the code is simple enough to avoid the risk associated with explicit array pointers. Also, `select rank` support requires Gfortran-10 or Intel Fortran 2020, so we didn't want to make too-new compiler restriction.

### Missing datatypes

* arrays of rank > 7: this has been stubbed in reader_nd.f90, writer_nd.f90. Only the latest compilers support Fortran 2008 arrays up to rank 15.

The datatypes below are more complex to handle and may see little use due to their downsides.

* complex64/complex128: this is not natively handled in HDF5. There are performance impacts for compound datatypes, thus many choose to just write two datasets, one each for real and imaginary like foo_r and foo_i
* non-default character kind
