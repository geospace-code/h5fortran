# h5fortran Examples

From the h5fortran/ directory, specify the h5fortran install like:

```sh
cmake -B build -DCMAKE_INSTALL_PREFIX=~/h5fortran
cmake --build build
cmake --install build

cmake -B example/build -S Examples -Dh5fortran_ROOT=~/h5fortran
```

## Example 1

Example 1 shows the functional one-step interface of h5fortran

## Example 2

Example 2 shows the object-oriented interface of h5fortran, which may offer faster performance if more than one variable is being read or written.

## Example 3

Example 3 is of a C main program calling a Fortran interface to h5fortran

## Example 4

Example 4 is of a C++ main program calling a Fortran interface to h5fortran

For a C++ header-only object-oriented HDF5 library, consider [HighFive](https://github.com/BlueBrain/HighFive)


## Notes

### Non CMake build

CMake makes building much easier.
If for whatever you don't wish to use CMake, the HDF5 compiler wrapper (if available on your system) may work.
Since the HDF5 compiler wrapper is not always working or available, we strongly recommended CMake as above for any HDF5-based application.

On Ubuntu it looks like:

```sh
$ h5fc -show

gfortran -I/usr/include/hdf5/serial -L/usr/lib/x86_64-linux-gnu/hdf5/serial /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5hl_fortran.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5_hl.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5_fortran.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.a -lpthread -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib/x86_64-linux-gnu/hdf5/serial
```
