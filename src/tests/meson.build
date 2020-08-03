test_minimal = executable('test_minimal', 'test_minimal.f90',
  dependencies: hdf5_interface)
test('minimal', test_minimal,
  args: meson.current_build_dir(),
  suite: 'h5basic',
  priority: 100,
  timeout: 10)

test_array = executable('test_array', 'test_array.f90', dependencies: hdf5_interface)
test('array', test_array, suite: 'h5basic', timeout: 10)

test_attr = executable('test_attributes', 'test_attributes.f90', dependencies: hdf5_interface)
test('attributes', test_attr, suite: 'h5basic', timeout: 10)

test_deflate = executable('test_deflate', 'test_deflate.f90', dependencies: hdf5_interface)
test('deflate', test_deflate, suite: 'h5basic', timeout: 10)

test_error = executable('test_error', 'test_error.f90', dependencies: hdf5_interface)
test('errors', test_error, suite: 'h5basic', timeout: 10)

test_exist = executable('test_exist', 'test_exist.f90', dependencies: hdf5_interface)
test('exist', test_exist, suite: 'h5basic', timeout: 10)

test_scalar = executable('test_scalar', 'test_scalar.f90', dependencies: hdf5_interface)
test('scalar', test_scalar, suite: 'h5basic', timeout: 10)

test_str = executable('test_string', 'test_string.f90', dependencies: hdf5_interface)
test('string', test_str, suite: 'h5basic', timeout: 10)

testh5 = executable('test_hdf5',
  sources: ['test_hdf5_ifc.f90', 'test_lt.f90'],
  dependencies: hdf5_interface,
  fortran_args: quiet)
test('h5interface', testh5, suite: 'h5basic', timeout: 15)

shapes = executable('shapes_hdf5', 'test_shape.f90', dependencies: hdf5_interface, fortran_args: quiet)

test('shape', shapes,
  is_parallel: false,
  suite: 'h5basic', priority: 50, timeout: 15)

python = find_program('python')

test('check_shape', python,
  args: [files('test_shape.py')],
  is_parallel: false,
  suite: 'h5shaky', priority: -100, timeout: 90)
