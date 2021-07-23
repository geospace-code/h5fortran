# Install h5fortran

h5fortran is typically built and installed with CMake or Meson.

## Requirements

* Fortran 2018 compiler (this project uses `submodule` and `error stop`). For example, Gfortran &ge; 7 or Intel oneAPI.
* HDF5 Fortran library (>= 1.8.7, including 1.10.x and 1.12.x)
  * MacOS / Homebrew: `brew install gcc hdf5`
  * Linux / Windows Subsystem for Linux: `apt install gfortran libhdf5-dev`
  * Windows MSYS2: `pacman -S mingw-w64-x86_64-hdf5`
  * build from source (optional): `python scripts/build_hdf5.py`

Note that some precompiled HDF5 libraries have only C / C++ without Fortran.

The library `libh5fortran` is built, link it into your program as usual along with the HDF5 libraries and include files.

## CMake

Build and self-test via:

```sh
cmake -B build
cmake --build build

# optional self-test
cd build
ctest
```

(optional) to install to a directory like ~/h5fortran:

```sh
cmake -B build -DCMAKE_INSTALL_PREFIX=~/h5fortran
cmake --install build
```

### use h5fortran from your project

```sh
cmake -B build -Dh5fortran_ROOT=~/h5fortran/
```

and in your CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.14...3.20)
project(myProject LANGUAGES Fortran)

find_package(h5fortran)

if(NOT h5fortran_FOUND)
  include(FetchContent)

  FetchContent_Declare(H5FORTRAN
    GIT_REPOSITORY https://github.com/geospace-code/h5fortran.git
    GIT_TAG v3.6.6)
  FetchContent_MakeAvailable(H5FORTRAN)
endif()

# --- your project targets:

add_executable(myProj main.f90)
target_link_libraries(myProj PRIVATE h5fortran::h5fortran)
```

and where the main.f90 is like:

```fortran
program main

use h5fortran, only : hdf5_file
implicit none

type(hdf5_file) :: h5f

call h5f%open('h5fortran_example2.h5', action='w')
call h5f%write('/x', 123)
call h5f%close()


end program
```

### [optional] create distributable archive

If you wish to create a package archive that is usable on systems with compatible Fortran ABI, after building:

```sh
cpack --config build/CPackConfig.cmake
```

### [optional] specify a particular HDF5 library

```sh
cmake -DHDF5_ROOT=/path/to/hdf5lib -B build
```

or set environment variable `HDF5_ROOT=/path/to/hdf5lib`

## Meson

To build h5fortran as a standalone project

```sh
meson build

meson test -C build
```

### h5fortran Meson subproject

To include h5fortran as a Meson subproject, in the main project meson.build (that uses h5fortran) have like:

```meson
hdf5_proj = subproject('h5fortran')
hdf5_interface = hdf5_proj.get_variable('hdf5_interface')

my_exe = executable('myexe', 'main.f90', dependencies: hdf5_interface)
```

and have a file in the main project `subprojects/h5fortran.wrap` containing:

```ini
[wrap-git]
directory = h5fortran
url = https://github.com/geospace-code/h5fortran.git
revision = v3.6.5
```

## Standalone compiler wrapper

h5fortran can be used from the
[HDF5 compiler wrapper "h5fc"](https://support.hdfgroup.org/HDF5/Tutor/compile.html) like:

```sh
h5fc -I~/h5fortran/include myprogram.f90 ~/h5fortran/lib/libh5fortran.a
```
