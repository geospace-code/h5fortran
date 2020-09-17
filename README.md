# Object-oriented Fortran 2008 HDF5 interface

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

Uses Fortran 2008 `submodule` for clean template structure.
This easy-to-use, thin object-oriented modern Fortran library abstracts away the messy parts of HDF5 so that you can read / write various types/ranks of data with a single command.
In distinction from other high-level HDF5 interfaces, h5fortran works to deduplicate code, using polymorphism wherever feasible and extensive test suite.

Polymorphic API with read/write for types int32, real32, real64 with rank:

* scalar (0-D)
* 1-D .. 7-D

as well as **character (string)**.

* HDF5 **attributes** are also supported for read/write with type character, int32, real32, real64.
* **Array slicing on read and write** is supported, that is, reading or writing part of a disk HDF5 array into a variable matching the slice shape.
* Mismatched datatypes are coerced as per standard Fortran rules. For example, reading a float HDF5 variable into an integer Fortran variable:  42.3 => 42
* Zlib (deflate) compression / decompression

Tested on systems with HDF5 1.8, 1.10 and 1.12 including:

* MacOS (homebrew)
* Linux
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

ctest -S h5fortran/setup.cmake -V
```

for more details see [Install.md](./Install.md)

### Auto-build HDF5

CMake for H5fortran is set to automatically build HDF5 on non-Windows platforms.
To disable this behavior use `cmake -Dautobuild=off` option.
HDF5 will be "installed" under "build/hdf5/" directory.
To permanently install the HDF5 library consider:

```sh
cd h5fortran/scripts

cmake -DCMAKE_INSTALL_PREFIX=~/lib -B build

cmake --build build

cmake --install build
```

or

```sh
python scripts/build_hdf5.py
```

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

On many platforms, you must first compile the HDF5 library yourself like:
```sh
python3 scripts/compile_hdf5.py
```

For Windows with Intel oneAPI,
[download and install](https://www.hdfgroup.org/downloads/hdf5/)
the file named like "hdf5-1.12.0-Std-win10_64-vs14-Intel.zip".
Note this will put the HDF5 /bin directory on PATH.
If not using Python with HDF5, leave it there so that the DLL's work properly at runtime--Windows does not have Rpath or LD_LIBRARY_PATH, just PATH.

If you also use HDF5 with Python in the same project, there will be conflicts between Python HDF on PATH and the HDF Group HDF5 library on Path.
What works in that case is taking HDF GRoup HDF5 out of PATH and configuring like:

```sh
cmake -B build -DHDF5_ROOT="C:/Program Files/HDF_Group/HDF5/1.12.0/"
```

### Missing datatypes

* arrays of rank > 7: this has been stubbed in reader_nd.f90, writer_nd.f90. Only the latest compilers support Fortran 2008 arrays up to rank 15.

The datatypes below are more complex to handle and may see little use due to their downsides.

* complex64/complex128: this is not natively handled in HDF5. There are performance impacts for compound datatypes, thus many choose to just write two datasets, one each for real and imaginary like foo_r and foo_i
* non-default character kind
