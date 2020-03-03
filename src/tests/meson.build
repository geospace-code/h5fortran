test_minimal = executable('test_minimal', 'test_minimal.f90',
  dependencies: hdf5_interface)
test('minimal', test_minimal,
  suite: 'h5basic',
  timeout: 10)

test_error = executable('test_error', 'test_error.f90',
  dependencies: hdf5_interface)
test('errors', test_error,
  args: meson.current_build_dir(),
  suite: 'h5basic',
  timeout: 10)

test_exist = executable('test_exist', 'test_exist.f90',
  dependencies: hdf5_interface)
test('exist', test_exist,
  args: meson.current_build_dir(),
  suite: 'h5basic',
  timeout: 10)

test_deflate = executable('test_deflate', 'test_deflate.f90',
  dependencies: hdf5_interface)
test('deflate', test_deflate,
  args: meson.current_build_dir(),
  suite: 'h5basic',
  timeout: 10)

testh5 = executable('test_hdf5',
  sources: ['test_hdf5_ifc.f90', 'test_lt.f90', 'test_array.f90', 'test_scalar.f90'],
  dependencies: hdf5_interface,
  fortran_args: quiet)
test('h5interface', testh5,
  args: meson.current_build_dir(),
  is_parallel: false,
  suite: 'h5basic',
  timeout: 15)

shapes = executable('shapes_hdf5', 'test_shape.f90',
  dependencies: hdf5_interface,
  fortran_args: quiet)

testfn = meson.current_build_dir()/'p5.h5'

test('Shapes', shapes,
  args: [testfn,'/group69/flux_node'],
  is_parallel: false,
  suite: 'h5basic',
  priority: -100,
  timeout: 15)

if get_option('test_shaky')
python = find_program('python')

test('CheckShapes', python,
  args: [files('test_shape.py'), testfn, '/group69/flux_node'],
  is_parallel: false,
  suite: 'h5shaky',
  priority: -100,
  timeout: 30)
endif