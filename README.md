[![DOI](https://zenodo.org/badge/128736984.svg)](https://zenodo.org/badge/latestdoi/128736984)

[![Actions Status](https://github.com/scivision/h5fortran/workflows/ci_linux/badge.svg)](https://github.com/scivision/h5fortran/actions)
[![Actions Status](https://github.com/scivision/h5fortran/workflows/ci_mac/badge.svg)](https://github.com/scivision/h5fortran/actions)

# Object-oriented Fortran 2018 HDF5 interface

Straightforward single-file/module access to HDF5.
Designed for easy use as a Meson "subproject" or CMake "ExternalProject".
Uses Fortran 2008 `submodule` and `error stop` for clean template structure.
This easy-to-use, thin object-oriented modern Fortran library abstracts away the messy parts of HDF5 so that you can read/write various types/ranks of data with a single command.

Polymorphic API with read/write for types integer, real32, real64 with rank:

* scalar (0-D)
* 1-D .. 7-D

as well as character (string) variables and attributes.

Tested on systems with HDF5 1.8 and 1.10 including:

* MacOS (homebrew)
* Ubuntu 16.04 / 18.04 (gfortran 6 or newer)
* Windows Subsystem for Linux.
* Windows MSYS2

Currently, Cygwin does not have *Fortran* HDF5 libraries.

## Build

Requirements:

* Fortran standard 2018 compiler (this project uses `submodule` and `error stop`)
* HDF5 Fortran library (1.8 or 1.10)
  * Mac / Homebrew: `brew install gcc hdf5`
  * Linux: `apt install gfortran libhdf5-dev`
  * Windows Subsystem for Linux: `apt install gfortran libhdf5-dev`
  * Windows MSYS2: `pacman -S mingw-w64-x86_64-hdf5`

Note that some precompiled HDF5 libraries include C / C++ without Fortran.

Build this HDF5 OO Fortran interface with Meson or CMake.
The library `libh5oo` is built, link it into your program as usual.

### Meson

```sh
meson build

meson test -C build
```

If HDF5 isn't found, you may need to specify on the command line:

* `-Dh5libdir`: HDF5 library directory
* `-Dh5incdir`: HDF5 include directory

To include h5fortran as a Meson subproject, in the master project meson.build (that uses h5fortran) have like:

```meson
hdf5_proj = subproject('h5fortran')
hdf5_interface = hdf5_proj.get_variable('hdf5_interface')

my_exe = exectuable('myexe', 'main.f90', dependencies: hdf5_interface)
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

To use h5fortran as a CMake ExternalProject do like:

```cmake
include(ExternalProject)

ExternalProject_Add(h5fortran
  GIT_REPOSITORY https://github.com/scivision/h5fortran.git
  GIT_TAG master  # it's better to use a specific Git revision or Git tag for reproducibility
  INSTALL_COMMAND ""  # disables the install step for the external project
)

ExternalProject_Get_Property(h5fortran BINARY_DIR)
set(h5fortran_BINARY_DIR BINARY_DIR)  # just to avoid accidentally reusing the variable name.

# your code "myio"
add_executable(myio myio.f90)
add_dependencies(myio h5fortran)
target_link_directories(myio PRIVATE ${h5fortran_BINARY_DIR})
```

## Usage

All examples assume:

```fortran
use hdf5_interface, only: hdf5_file
type(hdf5_file) :: h5f
```


* gzip compression may be applied for rank &ge; 2 arrays by setting `comp_lvl` to a value betwen 1 and 9.
  Shuffle filter is automatically applied for better compression
* string attributes may be applied to any variable at time of writing or later.
* `chunk_size` option may be set for better compression


### Create new HDF5 file, with variable "value1"

```fortran
call h5f%initialize('test.h5',status='new',action='w')

call h5f%add('/value1', 123.)

call h5f%finalize()
```

### Add/append variable "value1" to existing HDF5 file "test.h5"

* if file `test.h5` exists, add a variable to it
* if file `test.h5` does not exist, create it and add a variable to it.

```fortran
call h5f%initialize('test.h5', status='unknown',action='rw')

call h5f%add('/value1', 123.)

call h5f%finalize()
```

### Add gzip compressed 3-D array "value2" to existing HDF5 file "test.h5"

```fortran
real :: val2(1000,1000,3) = 0.

call h5f%initialize('test.h5', comp_lvl=1)

call h5f%add('/value2', val2)

call h5f%finalize()
```

chunk_size may optionally be set in the `%add()` method.

### Create group "scope"

```fortran
real :: val2(1000,1000,3) = 0.

call h5f%initialize('test.h5')

call h5f%add('/scope/')

call h5f%finalize()
```

## Permissive syntax

We make the hdf5%open(..., status=...) like Fortran open()

* overwrite (truncate) existing file: open with `status='new'` or `status='replace'`
* append to existing file or create file: `status='old'` or `status='unknown'`

Note the trailing `/` on `/scope/`, that tells the API you are creating a group instead of a variable.

## Notes

* The first character of the filename should be a character, NOT whitespace to avoid file open/creation errors.
* Using compilers like PGI or Flang may require first compiling the HDF5 library yourself.
* Intel compiler HDF5 [compile notes](https://www.hdfgroup.org/downloads/hdf5/source-code/)
* Polymorphic array rank is implemented by explicit code internally. We could have used pointers, but the code is simple enough to avoid the risk associated with explicit array pointers. Also, `select rank` support requires Gfortran-10 or Intel Fortran 2020, so we didn't want to make too-new compiler restriction.
