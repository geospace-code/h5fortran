[![Build Status](https://travis-ci.org/scivision/hdf5_interface.svg?branch=master)](https://travis-ci.org/scivision/hdf5_interface)


# HDF5 interface: Object-oriented Fortran

Very simple single-file/module access to HDF5.

Read/write integer/real:

* scalar
* 1-D
* 2-D
* 3-D

via polymorphism.



## Build

HDF5 1.10 is expected.

* Mac: `brew install gcc hdf5`
* Linux: `apt install gfortran libhdf5-dev`    (Ubuntu 18.04)
  For other/older Linux, you can use [Miniconda](https://conda.io/miniconda.html) to provide HDF5 by:
  ```sh
  conda install hdf5
  ```
* Windows: at this time, 
  [Scoop](https://www.scivision.co/brew-install-scoop-for-windows/) 
  didn't have HDF5, so consider using
  [Windows Subsystem for Linux](https://www.scivision.co/install-windows-subsystem-for-linux/)

and then:

```sh
cd app
cmake ../src
make

make test
```

## Usage

```fortran
use hdf5_interface, only: hdf5_file

implicit none

type(hdf5_file) :: h5f

call h5f%initialize('test.h5',status='NEW',action='WRITE')

call h5f%add('/value1', 123.0)

call h5f%finalize()
```
