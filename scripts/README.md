# Build HDF5 scripts

Here we use a dummy CMake project to reuse code from the main h5fortran project to build HDF5 and ZLIB.

```sh
cmake -B build -DCMAKE_INSTALL_PREFIX=~/local
cmake --build build
```

Optionally, build the MPI layer (parallel HDF5)

```sh
cmake -B build -DCMAKE_INSTALL_PREFIX=~/local -Dhdf5_parallel=on
cmake --build build
```

## compiler scripts

The "h5fc" and "h5cc" will be installed above under ~/local/bin.
These only appear on Linux and MacOS.
The HDF5 build system doesn't yet build these compiler wrappers on Windows.
