# Build HDF5 scripts

This folder can build HDF5 and ZLIB.
Pick CMAKE_INSTALL_PREFIX to be the directory you wish to install HDF5 under.

```sh
cmake -B build --install-prefix=$HOME/local
```

Optionally, build the MPI layer (parallel HDF5)

```sh
cmake -B build --install-prefix=$HOME/local -Dhdf5_parallel=on
```

Optionally, request a specific
[HDF5 release URL source archive](https://github.com/HDFGroup/hdf5/releases):

```sh
cmake -B build \
  -Dhdf5_url=https://github.com/HDFGroup/hdf5/archive/refs/tags/hdf5_1.14.4.3.tar.gz
```

Then, build and install:

```sh
cmake --build build
cmake --install build
```

## compiler wrappers

The HDF5 compiler wrappers will be installed under the install_prefix/bin for macOS and Linux.

* non-MPI (default): `h5cc` (C), `h5fc` (Fortran)
* MPI: `h5pcc` (C), `h5pfc` (Fortran)
