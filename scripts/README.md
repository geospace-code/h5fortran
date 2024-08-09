# Build HDF5 scripts

This folder can build HDF5 and ZLIB.
Pick CMAKE_INSTALL_PREFIX to be the directory you wish to install HDF5 under.

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

The HDF5 compiler wrappers will be installed above under ~/local/bin for macOS and Linux.

* non-MPI (default): `h5cc` (C), `h5fc` (Fortran)
* MPI: `h5pcc` (C), `h5pfc` (Fortran)
