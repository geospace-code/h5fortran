[![Build Status](https://travis-ci.org/scivision/hdf5_interface.svg?branch=master)](https://travis-ci.org/scivision/hdf5_interface)

# HDF5 interface: Object-oriented Fortran

Very simple single-file/module access to HDF5.
More [advanced object-oriented Fortran HDF5 access](https://github.com/rjgtorres/oo_hdf) also available.

Read/write integer / real32/64:

* scalar
* 1-D
* 2-D
* 3-D

via polymorphism.



## Build

HDF5 1.10 is expected.

* Mac: `brew install gcc hdf5`
* Linux: `apt install gfortran libhdf5-dev`    (Ubuntu 18.04)
  For older Linux, you can use [Miniconda](https://conda.io/miniconda.html) to provide HDF5 by:
  ```sh
  conda install hdf5
  ```
  Yes, with Miniconda in your PATH, CMake will find its HDF5 library.
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
