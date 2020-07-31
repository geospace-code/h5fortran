# h5fortran Example project

It's easiest to use CMake with h5fortran, since HDF5 consists of many library files and headers.

```sh
cmake -B build
cmake --build build
```

which creates build/example1[.exe]

If you do need to use a command line, the HDF5 compiler wrapper (if available on your system) may work.
Since the HDF5 compiler wrapper is not always working or available, we strongly recommended CMake as above for any HDF5-based application.

On Ubuntu it looks like:

```sh
$ h5fc -show

gfortran -I/usr/include/hdf5/serial -L/usr/lib/x86_64-linux-gnu/hdf5/serial /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5hl_fortran.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5_hl.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5_fortran.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.a -lpthread -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib/x86_64-linux-gnu/hdf5/serial
```
