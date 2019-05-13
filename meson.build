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

ooh5 = library('oohdf5', hdf5_src, dependencies: hdf5)

# --- testing
subdir(tests)

shapes = executable('shapes_hdf5', 'src/GetShape.f90',
  dependencies: hdf5,
  link_with: ooh5,
  fortran_args: quiet)
test('Shapes', shapes, args: ['p5.h5','group69/flux_node'], is_parallel: false)
