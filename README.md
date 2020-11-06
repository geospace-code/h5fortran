# Object-oriented Fortran HDF5 interface

[![DOI](https://zenodo.org/badge/128736984.svg)](https://zenodo.org/badge/latestdoi/128736984)
[![CDash](./archive/cdash.png)](https://my.cdash.org/index.php?project=h5fortran)
![ci_linux](https://github.com/geospace-code/h5fortran/workflows/ci_linux/badge.svg)
![ci_mac](https://github.com/geospace-code/h5fortran/workflows/ci_mac/badge.svg)
![ci_windows](https://github.com/geospace-code/h5fortran/workflows/ci_windows/badge.svg)

Simple, robust, thin HDF5 polymorphic Fortran read/write interface.
Reading or writing {real64,real32,int32} from scalar to 7d is as simple as

```fortran
use h5fortran

call h5write('foo.h5', '/x', x)

call h5read('bar.h5', '/y', y)
```

* For NetCDF4 see [nc4fortran](https://github.com/geospace-code/nc4fortran/).
* Designed for easy use using **static** or **shared** linking via:
  * CMake ExternalProject
  * CMake FetchContent
  * CMake + Git submodule
  * Meson subproject

Uses Fortran `submodule` for clean template structure.
This easy-to-use, thin object-oriented modern Fortran library abstracts away the messy parts of HDF5 so that you can read / write various types/ranks of data with a single command.
In distinction from other high-level HDF5 interfaces, h5fortran works to deduplicate code, using polymorphism wherever feasible and extensive test suite.

Polymorphic API with read/write for types int32, real32, real64 with rank:

* scalar (0-D)
* 1-D .. 7-D

as well as **character (string)**.

* HDF5 **attributes** are also supported for read/write with type character, int32, real32, real64.
* **Array slicing on read and write** is supported, that is, reading or writing part of a disk HDF5 array into a variable matching the slice shape.
* Mismatched datatypes are coerced as per standard Fortran rules. For example, reading a float HDF5 variable into an integer Fortran variable:  42.3 => 42
* Zlib (deflate) compression / decompression -- h5fortran will work without Zlib, but will save/load uncompressed data only.

Tested on systems with HDF5 1.8, 1.10 and 1.12 including:

* MacOS (homebrew)
* Linux (Ubuntu, CentOS)
* Windows Subsystem for Linux
* Windows MSYS2
* IBM Power with Gfortran

Compilers known to work include:

* Gfortran &ge; 6
* Intel oneAPI HPC compiler

## Build

Using CMake:

```sh
git clone https://github.com/geospace-code/h5fortran.git

ctest -S h5fortran/setup.cmake -VV
```

for more details see [Install.md](./Install.md)

### Autobuild HDF5

h5fortran will automatically build the HDF5 library if needed.
This takes a minute or two on a typical laptop.

## Usage

The simplest [example](./Examples/) h5fortran usage is like:

```fortran
use h5fortran

call h5write('golt.h5','/x', [1,2,3,4,5,6])
```

or

```fortran
use h5fortran

real :: x2

if(.not. is_hdf5('golt.h5')) error stop 'golt.h5 is not an HDF5 file'

call h5read('golt.h5', '/x', x2)
```

For detailed [examples](./Examples/) see [Examples.md](./Examples.md).

## Notes

* The first character of the filename should be a character, NOT whitespace to avoid file open/creation errors.
* Polymorphic array rank is implemented.

### Getting HDF5 library

Instead of auto-building HDF5 via H5Fortran, one may build and install the HDF5 library by:

```sh
python3 scripts/build_hdf5.py
```

### Missing datatypes

* arrays of rank > 7: this has been stubbed in reader_nd.f90, writer_nd.f90. Only the latest compilers support Fortran 2008 arrays up to rank 15.

The datatypes below are more complex to handle and may see little use due to their downsides.

* complex64/complex128: this is not natively handled in HDF5. There are performance impacts for compound datatypes, thus many choose to just write two datasets, one each for real and imaginary like foo_r and foo_i
* non-default character kind
