project('h5fortran', 'fortran',
  meson_version : '>=0.52.0',
  version : '1.4.1',
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
  args: [meson.build_root()/'p5.h5','/group69/flux_node'],
  is_parallel: false,
  suite: 'h5fortran',
  priority: -100,
  timeout: 15)

python = find_program('python')

test('CheckShapes', python,
  args: [files('tests/GetShapes.py'), meson.build_root()/'p5.h5', '/group69/flux_node'],
  is_parallel: false,
  suite: 'h5fortran',
  priority: -100,
  timeout: 30)