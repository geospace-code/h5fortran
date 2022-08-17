src_dir: ./src
output_dir: ./docs
project: Object-oriented Fortran HDF5 interface
project_github: https://github.com/geospace-code/h5fortran
project_website: https://geospace-code.github.io/h5fortran
summary: Object-oriented Fortran HDF5 interface
author: Michael Hirsch, Ph.D.
github: https://github.com/geospace-code
license: by
exclude: CMakeFortranCompilerId.F
         reader_lt_template.in.f90
         writer_lt_template.in.f90
         writer_template.in.f90
         writer_template_i32.in.f90
         writer_template_r64.in.f90
         writer_template_r32.in.f90
         writer_template_i64.in.f90
display: public
         protected
         private
source: false
graph: true
search: true


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

h5fortran is designed for easy use using **static** or **shared** linking from your project via:

* `cmake --install`
* CMake ExternalProject
* CMake FetchContent
* CMake + Git submodule
* Meson subproject

Uses Fortran `submodule` for clean template structure.
This easy-to-use, thin object-oriented modern Fortran library abstracts away the messy parts of HDF5 so that you can read / write various types/ranks of data with a single command.
In distinction from other high-level HDF5 interfaces, h5fortran works to deduplicate code, using polymorphism wherever feasible and extensive test suite.

Polymorphic [API](./API.md) with read/write types int32, int64, real32, real64, character with rank scalar (0-D) through 7-D.
If you need int64, we have a working example for that: src/concepts/int64.f90 that can easily be put into the h5fortran API--just make a GitHub Issue.

* HDF5 **attributes** are also supported for read/write types int32, int64, real32, real64, character with rank scalar (0-D) through 7-D.
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

Compilers known to work include:

* GCC (gfortran) &ge; 7
* Intel oneAPI HPC compiler &ge; 2021

---

We welcome [contributions](https://github.com/geospace-code/.github/blob/main/CONTRIBUTING.md).
In general we hold to the geospace-code [code of conduct](https://github.com/geospace-code/.github/blob/main/CODE_OF_CONDUCT.md).

## Build

Using CMake:

```sh
git clone https://github.com/geospace-code/h5fortran.git

cd h5fortran

cmake -B build

cmake --build build
```

for more details see [Install.md](./Install.md).

For general use with non-CMake build systems, "h5fortran.pc" pkg-config file is also generated / installed.

### Autobuild HDF5

h5fortran will automatically build the HDF5 and ZLIB libraries if needed.
This is useful as many HPC have broken or ABI-incompatible HDF5 libraries installed.
Building HDF5 and ZLIB takes about a minute on a typical laptop.
To disable this autobuild behavior, use option:

```sh
cmake -B build -Dautobuild=off
```

To force building the HDF5 and ZLIB libraries, to gain better performance via optimizing for your system's CPU:

```sh
cmake -Dhdf5_external=on

cmake --build build
```

NOTE: If using Intel oneAPI on Windows, ensure that environment variable CC=icl as set manually in the command prompt:

```posh
set CC=icl
set FC=ifort
```

This is necessary to workaround techniques used by HDF5 CMake files that don't pickup the CMake `set(ENV{CC})`.
Otherwise, HDF5 build failures may result due to defaulting to icl-clang.

By default we use Zlib 2.x a.k.a. zlib-ng.
If you have a problem with Zlib-ng on your system, try the unmaintained Zlib 1.x by:

```sh
cmake -B build -Dzlib_legacy=on
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

### Getting HDF5 library

Instead of auto-building HDF5 via H5Fortran, one may build and install the HDF5 library by:

```sh
python3 scripts/build_hdf5.py
```

### h5fortran: missing Fortran datatypes

* arrays of rank > 7: prototyped in reader_nd.f90, writer_nd.f90. Fortran 2008 arrays are up to rank 15, but only recent compilers support.
* complex64 / complex128: not natively handled in HDF5. There are performance impacts for compound datatypes. Many choose to write two datasets, one each for real and imaginary like `A_real` and `A_imag`
* non-default character kind
* logical / boolean: not supported natively by HDF5. h5py implements as [enum](https://docs.h5py.org/en/stable/faq.html#what-datatypes-are-supported).
