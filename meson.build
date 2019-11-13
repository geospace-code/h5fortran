project('h5fortran', 'fortran',
  meson_version : '>=0.51.2',
  version : '1.3.1',
  default_options : ['default_library=static', 'buildtype=release', 'warning_level=3'])

subdir('meson')

subdir('src')

ooh5 = library('h5fortran',
  sources: hdf5_src,
  dependencies: hdf5,
  install: true)

hdf5_interface = declare_dependency(link_with: ooh5, dependencies: hdf5)

# --- testing
subdir('tests')

shapes = executable('shapes_hdf5', 'tests/GetShape.f90',
  dependencies: hdf5_interface,
  fortran_args: quiet)

test('Shapes', shapes,
  args: ['p5.h5','group69/flux_node'],
  is_parallel: false,
  timeout: 30)
