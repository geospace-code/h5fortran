# Object-oriented Fortran HDF5 interface

[![DOI](https://joss.theoj.org/papers/10.21105/joss.02842/status.svg)](https://doi.org/10.21105/joss.02842)
[![DOI](https://zenodo.org/badge/128736984.svg)](https://zenodo.org/badge/latestdoi/128736984)

[![ci](https://github.com/geospace-code/h5fortran/actions/workflows/ci.yml/badge.svg)](https://github.com/geospace-code/h5fortran/actions/workflows/ci.yml)
[![ci_build](https://github.com/geospace-code/h5fortran/actions/workflows/ci_build.yml/badge.svg)](https://github.com/geospace-code/h5fortran/actions/workflows/ci_build.yml)
[![oneapi-linux](https://github.com/geospace-code/h5fortran/actions/workflows/oneapi-linux.yml/badge.svg)](https://github.com/geospace-code/h5fortran/actions/workflows/oneapi-linux.yml)
[![ci_fpm](https://github.com/geospace-code/h5fortran/actions/workflows/ci_fpm.yml/badge.svg)](https://github.com/geospace-code/h5fortran/actions/workflows/ci_fpm.yml)

Simple, robust, thin HDF5 polymorphic Fortran read/write interface.
Reading or writing {real64,real32,int32,int64} from scalar to 7d is as simple as

```fortran
use h5fortran

call h5write('my.h5', '/x', x)

call h5read('my.h5', '/y', y)
```

For NetCDF4 see [nc4fortran](https://github.com/geospace-code/nc4fortran/).
h5fortran is designed for "serial" HDF5 read/write.
We don't yet implement the interface for "parallel" HDF5.

h5fortran is designed for easy use using static or shared linking from your project via:

* `cmake --install`
* CMake [ExternalProject (example)](https://gist.github.com/scivision/dd0e7aa18ba1397b0274c82603e4650c)
* CMake [FetchContent (example)](https://gist.github.com/scivision/671633df71c5aaef7866d0c3a1504116)
* CMake + Git submodule
* Fortran Package Manager (fpm)

Uses Fortran `submodule` for clean template structure.
This easy-to-use, thin object-oriented modern Fortran library abstracts away the messy parts of HDF5 so that you can read / write various types/ranks of data with a single command.
In distinction from other high-level HDF5 interfaces, h5fortran works to deduplicate code, using polymorphism wherever feasible and extensive test suite.

Polymorphic [API](./API.md) with read / write **dataset** types int32, int64, real32, real64, character (string) with rank scalar (0-D) through 7-D.
Variable-length datasets "H5S_UNLIMITED" can also be read.

HDF5 **attributes** are also supported for read/write types int32, int64, real32, real64, character (string) with rank scalar (0-D) through 7-D.

For **character**, datasets and attributes are supported for fixed length and variable length strings.
Character padding uses spaces as this is the default for Fortran.
Space-padded, null-terminated and null-padded strings are also supported on read.
UTF8 characters are passed through on read and write using the Fortran default character kind.

* **Array slicing on read and write** is supported, that is, reading or writing part of a disk HDF5 array into a variable matching the slice shape.
* Mismatched datatypes are coerced as per standard Fortran rules. For example, reading a float HDF5 variable into an integer Fortran variable:  42.3 => 42
* Zlib (deflate) compression / decompression -- h5fortran will work without Zlib, but will save/load uncompressed data only.
* create HDF5 soft link variables--arbitrarily many soft-linked variable names can point to an actual variable, which need not yet exist.

Tested on systems with HDF5 1.8, 1.10 and 1.12 including:

* MacOS (homebrew)
* Linux (Ubuntu, CentOS)
* Windows Subsystem for Linux
* Windows MSYS2
* IBM Power with Gfortran
* Cray (using GCC or Intel backend)

Compilers known to work (tested on CI) include:

* GCC (gfortran) &ge; 8
* Intel oneAPI &ge; 2021

Compilers not currently working:

* NVidia HPC SDK 23.11: missing "rank()" intrinsic and trouble with "select type()" in submodule
* AOCC (same problems as NVHPC 23.11)

---

We welcome [contributions](https://github.com/geospace-code/.github/blob/main/CONTRIBUTING.md).
In general we hold to the geospace-code [code of conduct](https://github.com/geospace-code/.github/blob/main/CODE_OF_CONDUCT.md).

## Build

h5fortran can be built with CMake or [fpm](https://fpm.fortran-lang.org/).

### CMake

Using CMake:

```sh
git clone https://github.com/geospace-code/h5fortran.git

cd h5fortran

cmake -B build

cmake --build build
```

for more details see [Install.md](./Install.md).

For general use with non-CMake build systems, "h5fortran.pc" pkg-config file is also generated / installed.

To save time, if not intended to use self-tests, you can skip the build of the test suite:

```sh
cmake -B build -Dh5fortran_BUILD_TESTING=off
```

### Fortran Package Manager (fpm)

```sh
fpm build
fpm test
fpm install
```

## Build HDF5

To build the HDF5 and ZLIB libraries:

```sh
cmake -S scripts -B scripts/build -DCMAKE_INSTALL_PREFIX=~/mylibs

cmake --build scripts/build
```

Then build h5fortran:

```sh
cmake -B build -DCMAKE_PREFIX_PATH=~/mylibs

cmake --build build
```

Request a specific [HDF5 Git tag](https://github.com/HDFGroup/hdf5/tags) or commit hash like:

```sh
cmake -B build -Dhdf5_tag=hdf5-1_12_2
```

## Usage

The simplest [example](./example/) h5fortran usage is like:

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

For detailed [examples](./example/) see [Examples.md](./Examples.md).

## Notes

* The first character of the filename should be a character, NOT whitespace to avoid file open/creation errors.
* Polymorphic array rank is implemented.

### h5fortran: missing HDF5 features

* arrays of rank > 7: prototyped in reader_nd.f90, writer_nd.f90. Fortran 2008 arrays are up to rank 15, but only recent compilers support.
* complex64 / complex128: not natively handled in HDF5. There are performance impacts for compound datatypes. Many choose to write two datasets, one each for real and imaginary like `A_real` and `A_imag`
* non-default character kind -- we do pass-through UTF8
* logical / boolean: not supported natively by HDF5. h5py implements as [enum](https://docs.h5py.org/en/stable/faq.html#what-datatypes-are-supported).
* variable length dataset writing

We didn't use `type(c_ptr)` and `c_loc()` internally for datasets as we observed problems when the actual argument is sliced on read/write.
The current h5fortran impementation (Fortran `select type` for H5Dwrite/H5Dread) does work with sliced actual arguments.

HDF5 Fortran 2003 [features](https://docs.hdfgroup.org/archive/support/HDF5/doc/fortran/NewFeatures_F2003.pdf)
