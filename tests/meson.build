test_error = executable('test_error', 'test_error.f90',
  dependencies: hdf5_interface)
test('errors', test_error,
  args: meson.current_build_dir(),
  suite: 'h5basic',
  timeout: 10)


testh5 = executable('test_hdf5', 'test_hdf5_ifc.f90',
  dependencies: hdf5_interface,
  fortran_args: quiet)
test('h5interface', testh5,
  is_parallel: false,
  suite: 'h5basic',
  timeout: 15)

shapes = executable('shapes_hdf5', 'GetShape.f90',
  dependencies: hdf5_interface,
  fortran_args: quiet)

testfn = meson.build_root()/'p5.h5'

test('Shapes', shapes,
  args: [testfn,'/group69/flux_node'],
  is_parallel: false,
  suite: 'h5shaky',
  priority: -100,
  timeout: 15)

python = find_program('python')

test('CheckShapes', python,
  args: [files('GetShapes.py'), testfn, '/group69/flux_node'],
  is_parallel: false,
  suite: 'h5shaky',
  priority: -100,
  timeout: 30)