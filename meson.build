project('Object-oriented HDF5 Fortran', 'fortran',
  meson_version : '>=0.50',
  version : '1.0.0')

add_global_arguments('-g', language : 'fortran')

fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
  add_global_arguments( '-O', '-fimplicit-none', '-Wextra', '-Wpedantic', language : 'fortran')
  quiet = ['-Wno-compare-reals', '-Wno-maybe-uninitialized']
else
  quiet = ['-w']
endif

hdf5 = dependency('hdf5', language : 'fortran', required : true)

h5run = fc.run('use h5lt; end', dependencies: hdf5)
if h5run.returncode() != 0
  error('HDF5 linking problems: ' + h5run.stderr())
endif

ooh5 = library('oohdf5', 'src/hdf5_interface.f90','src/read.f90', 'src/read_real64.f90', 'src/read_real32.f90', 'src/read_int.f90', 'src/write.f90', 'src/write_real64.f90', 'src/write_real32.f90', 'src/write_int.f90',
  dependencies: hdf5)

# --- testing
testh5 = executable('test_hdf5', 'src/test_hdf5_ifc.f90',
  dependencies: hdf5,
  link_with: ooh5,
  fortran_args: quiet)
test('Test HDF5 OO', testh5, is_parallel: false)

shapes = executable('shapes_hdf5', 'src/GetShape.f90',
  dependencies: hdf5,
  link_with: ooh5,
  fortran_args: quiet)
test('Shapes', shapes, args: ['p5.h5','group69/flux_node'], is_parallel: false)
