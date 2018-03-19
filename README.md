[![Build Status](https://travis-ci.org/scivision/hdf5_interface.svg?branch=master)](https://travis-ci.org/scivision/hdf5_interface)
[![Build status](https://ci.appveyor.com/api/projects/status/9c0c6adudwyrg9yv?svg=true)](https://ci.appveyor.com/project/scivision/hdf5-interface)

# Object-oriented Fortran 2018 HDF5 interface

Very simple single-file/module access to HDF5.
More [advanced object-oriented Fortran HDF5 access](https://github.com/rjgtorres/oo_hdf) also available.

Read/write integer / real32/64:

* scalar
* 1-D
* 2-D
* 3-D

via polymorphism.

Tested on systems including Mac OS X (via homebrew), Ubuntu 16.04/18.04 (gfortran &ge; 5.5) with HDF5 1.8 and 1.10 and Windows Subsystem for Linux.


## Build

Requirements:

* modern Fortran 2018 compiler (such as gfortran &ge; 5.5, etc.)
* HDF5 library (1.8 or 1.10)

* Mac: `brew install gcc hdf5`
* Linux: `apt install gfortran libhdf5-dev`
* Windows: at this time,
  [Scoop](https://www.scivision.co/brew-install-scoop-for-windows/)
  didn't have HDF5, possibly due to
  [difficulties with HDF5 and gfortran on Windows](https://stackoverflow.com/a/30056831),
  so consider using
  [Windows Subsystem for Linux](https://www.scivision.co/install-windows-subsystem-for-linux/)

and then:

```sh
cd app
cmake ../src
make

make test
```

## Usage

Here are a few examples.
All of them assume:

```fortran
use hdf5_interface, only: hdf5_file
type(hdf5_file) :: h5f
```


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
