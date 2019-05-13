[![DOI](https://zenodo.org/badge/128736984.svg)](https://zenodo.org/badge/latestdoi/128736984)
[![Build Status](https://travis-ci.org/scivision/oo_hdf5_fortran.svg?branch=master)](https://travis-ci.org/scivision/oo_hdf5_fortran)
[![Build status](https://ci.appveyor.com/api/projects/status/9njjb04mol8l2sjx?svg=true)](https://ci.appveyor.com/project/scivision/oo-hdf5-fortran)

# Object-oriented Fortran 2018 HDF5 interface

Straightforward single-file/module access to HDF5.
Uses Fortran 2008 `submodule` for clean, templatable structure.
This thin object-oriented modern Fortran library abstracts away the messy parts of HDF5 so that you can read/write various types/ranks of data with a single command.

Polymorphic API with read/write for types integer, real32, real64 with rank:

* scalar (0-D)
* 1-D .. 7-D

as well as character (string) variables and attributes.

Tested on systems including

* MacOS (via homebrew)
* Ubuntu 16.04/18.04 (gfortran &ge; 5.4.1) with HDF5 1.8 and 1.10
* Windows Subsystem for Linux.

Note: Currently, Cygwin does not have Fortran HDF5 libraries.

## Build

Requirements:

* modern Fortran 2008 compiler (such as gfortran &ge; 5.4.1, etc.)
* HDF5 library (1.8 or 1.10)
  * Mac: `brew install gcc hdf5`
  * Linux: `apt install gfortran libhdf5-dev`
  * Windows: at this time,
    [Scoop](https://www.scivision.co/brew-install-scoop-for-windows/) and Chocolatey
    do not have HDF5, possibly due to difficulties with
    [HDF5 and gfortran on Windows](https://stackoverflow.com/a/30056831).
    Consider using
    [Windows Subsystem for Linux](https://www.scivision.co/install-windows-subsystem-for-linux/).


Build this HDF5 OO Fortran interface with other Meson or CMake.
The library `libh5oo` is built, link it into your program as usual.

### Meson

```sh
meson build

ninja -C build

ninja test -C build
```

### CMake

```sh
cd build
cmake ..

cmake --build .

ctest -V
```
If you need to specify a particular HDF5 library, use `cmake -DHDF5_ROOT=/path/to/hdf5lib ..`


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

### Add variable "value1" to existing HDF5 file "test.h5"

```fortran
call h5f%initialize('test.h5',status='old',action='rw')

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

Note the trailing `/` on `/scope/`, that tells the API you are creating a group instead of a variable.
## Notes

* The first character of the filename should be a character, NOT whitespace to avoid file open/creation errors.
* Using compilers like PGI or Flang may require first compiling the HDF5 library yourself.
* Polymorphic array rank is implemented by explicit code internally. We could have used pointers, but the code is simple enough to avoid the risk associated with explicit array pointers.
