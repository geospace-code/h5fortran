# Install h5fortran

h5fortran is typically built and installed with CMake or Meson.

## Requirements

* modern Fortran compiler (this project uses `submodule` and `error stop`). For example, Gfortran &ge; 6 or Intel &ge; 19.0.
* HDF5 Fortran library (>= 1.8.7, including 1.10.x and 1.12.x)
  * MacOS / Homebrew: `brew install gcc hdf5`
  * Linux / Windows Subsystem for Linux: `apt install gfortran libhdf5-dev` or `python scripts/compile_hdf5.py`
  * Windows MSYS2: `pacman -S mingw-w64-x86_64-hdf5`

Note that some precompiled HDF5 libraries include C / C++ without Fortran.
Platforms that currently do **not** have Fortran HDF5 libraries, and thus will **not** work with h5fortran unless you compile HDF5 library for Fortran include:

* Cygwin
* Conda

Build this HDF5 OO Fortran interface with Meson or CMake.
The library `libh5fortran` is built, link it into your program as usual along with the HDF5 libraries and include files.
It's generally recommended you use a metabuild system with HDF5 (CMake or Meson) unless you are experience with Makefiles and need to use them.

If you use
[conan](https://conan.io),
get the prereqs and build by:

```sh
conan install . -if build

conan build . -bf build
```

## CMake

Build and self-test via:

```sh
ctest -S setup.cmake -V
```

### [optional] install

To install h5fortran to a directory, to use in many programs do like:

```sh
cmake -B build -DCMAKE_INSTALL_PREFIX=~/lib

cmake --build build

cmake --install build
```

### [optional] create distributable archive

If you wish to create a .zip archive that is usable on systems with compatible Fortran ABI:

```sh
ctest -S setup.cmake

cd build

cpack
```

### [optional] specify a particular HDF5 library

```sh
cmake -DHDF5_ROOT=/path/to/hdf5lib -B build
```

or set environment variable `HDF5_ROOT=/path/to/hdf5lib`

### use CMake target `h5fortran` via CMake FetchContent

```cmake
include(FetchContent)

FetchContent_Declare(h5fortran_proj
  GIT_REPOSITORY https://github.com/geospace-code/h5fortran.git
  GIT_TAG master  # whatever desired version is
)

FetchContent_MakeAvailable(h5fortran_proj)

# ------------------------------------------------------
# whatever your program is
add_executable(myProj main.f90)
target_link_libraries(myProj h5fortran::h5fortran)
```

## Meson

To build h5fortran as a standalone project

```sh
meson build

meson test -C build
```

Meson &ge; 0.53.0 has enhanced HDF5 dependency finding and is recommended.

### h5fortran Meson subproject

To include h5fortran as a Meson subproject, in the master project meson.build (that uses h5fortran) have like:

```meson
hdf5_proj = subproject('h5fortran')
hdf5_interface = hdf5_proj.get_variable('hdf5_interface')

my_exe = executable('myexe', 'main.f90', dependencies: hdf5_interface)
```

and have a file in the master project `subprojects/h5fortran.wrap` containing:

```ini
[wrap-git]
directory = h5fortran
url = https://github.com/geospace-code/h5fortran.git
revision = head
```
