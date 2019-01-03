project('Object-oriented HDF5 Fortran', 'fortran',
  version : '1.0.0')

add_global_arguments('-g', language : 'fortran')

fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
  add_global_arguments( '-fimplicit-none', '-Wall', '-Wextra', '-Wpedantic', language : 'fortran')
endif

libhdf5 = fc.find_library('hdf5', required : true)

library('oohdf5', 'src/hdf5_interface.f90','src/read.f90', 'src/read_real64.f90', 'src/read_real32.f90', 'src/read_int.f90', 'src/write.f90', 'src/write_real64.f90', 'src/write_real32.f90', 'src/write_int.f90')

# --- testing
executable('test_hdf5', 'src/test_hdf5_ifc.f90', libhdf5)

executable('shapes_hdf5', 'src/GetShape.f90', libhdf5)
