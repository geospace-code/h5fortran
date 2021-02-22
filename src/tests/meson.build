test_minimal = executable('test_minimal', 'test_minimal.f90',
  dependencies: hdf5_interface)
test('minimal', test_minimal,
  args: meson.current_build_dir(),
  suite: 'h5basic',
  priority: 100,
  timeout: 10)

foreach t : ['array', 'attributes', 'deflate', 'error', 'exist', 'module', 'layout', 'lt', 'scalar', 'shape', 'string',
'fail_read_size_mismatch', 'fail_read_rank_mismatch', 'fail_nonexist_variable', 'fail_unknown_read', 'fail_unknown_write']

  e = executable('test_'+t, 'test_' + t + '.f90', dependencies: hdf5_interface)
  if t == 'shape'
    par = false
    pri = 50
  else
    par = true
    pri = 0
  endif
  test(t, e, suite: 'h5basic', timeout: 10, is_parallel: par, priority: pri,
       workdir : meson.current_build_dir(),
       should_fail : t.contains('fail'))
endforeach

python = find_program('python')

test('check_shape', python,
  args: [files('test_shape.py'), meson.current_build_dir() / 'test_shape.h5'],
  is_parallel: false,
  suite: 'h5shaky', priority: -100, timeout: 90)
