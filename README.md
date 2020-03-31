# Object-oriented Fortran 2008 HDF5 interface

[![DOI](https://zenodo.org/badge/128736984.svg)](https://zenodo.org/badge/latestdoi/128736984)
[![CDash](./archive/cdash.png)](https://my.cdash.org/index.php?project=h5fortran)

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
* Designed for easy use using **static** or **shared** linking via:
  * CMake ExternalProject
  * CMake FetchContent
  * CMake + Git submodule
  * Meson subproject

Uses Fortran 2008 `submodule` for clean template structure.
This easy-to-use, thin object-oriented modern Fortran library abstracts away the messy parts of HDF5 so that you can read / write various types/ranks of data with a single command.
In distinction from other high-level HDF5 interfaces, h5fortran works to deduplicate code, using polymorphism wherever feasible and extensive test suite.

Polymorphic API with read/write for types int32, real32, real64 with rank:

* scalar (0-D)
* 1-D .. 7-D

as well as character (string) variables and attributes.

Array slicing on read is supported, that is, reading part of a disk HDF5 array into a variable of matching shape.

Mismatched datatypes are coerced as per standard Fortran rules.
For example, reading a float HDF5 variable into an integer Fortran variable:  42.3 => 42

Tested on systems with HDF5 1.8, 1.10 and 1.12 including:

* MacOS (homebrew)
* Linux
* Windows Subsystem for Linux
* Windows MSYS2
* IBM Power with Gfortran

Compilers known to work include:

* Gfortran >= 6
* Intel compiler

## Install

Using CMake:

```sh
ctest -S setup.cmake -V
```

for more details see [Install.md](./Install.md)

If you need to compile the HDF5 library consider:

```sh
python3 scripts/compile_hdf5.py
```

## Usage

The simplest example h5fortran usage is like:

```fortran
use h5fortran

real :: x, x2

call h5write('golt.h5','/x', [1,2,3,4,5,6])

call h5read('golt.h5', '/x', x2)
```

For detailed examples see [Examples.md](./Examples.md).

## Notes

* The first character of the filename should be a character, NOT whitespace to avoid file open/creation errors.
* [PGI](https://www.fluidnumerics.com/resources/building-hdf5-with-pgi) or Flang compilers require first compiling the HDF5 library yourself.
* Polymorphic array rank is implemented by explicit code internally. We could have used pointers, but the code is simple enough to avoid the risk associated with explicit array pointers. Also, `select rank` support requires Gfortran-10 or Intel Fortran 2020, so we didn't want to make too-new compiler restriction.

### Missing datatypes

* arrays of rank > 7: this has been stubbed in reader_nd.f90, writer_nd.f90. Only the latest compilers support Fortran 2008 arrays up to rank 15.

The datatypes below are more complex to handle and may see little use due to their downsides.

* complex64/complex128: this is not natively handled in HDF5. There are performance impacts for compound datatypes, thus many choose to just write two datasets, one each for real and imaginary like foo_r and foo_i
* non-default character kind
