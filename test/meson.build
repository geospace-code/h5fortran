test_minimal = executable('test_minimal', 'test_minimal.f90',
  dependencies: hdf5_interface)
test('minimal', test_minimal,
  args: meson.current_build_dir(),
  suite: 'unit',
  priority: 100,
  timeout: 10)

foreach t : ['array', 'attributes', 'cast',
'deflate_write', 'deflate_read', 'deflate_props',
'destructor', 'exist', 'groups', 'layout', 'lt',
'scalar', 'shape', 'string', 'version', 'write',
'fail_read_size_mismatch', 'fail_read_rank_mismatch', 'fail_nonexist_variable']

  e = executable('test_'+t, 'test_' + t + '.f90', dependencies: hdf5_interface)
  if ['shape', 'deflate_write', 'write'].contains(t)
    par = false
    pri = 50
  else
    par = true
    pri = 0
  endif
  test(t, e, suite: 'unit', timeout: 10, is_parallel: par, priority: pri,
       workdir : meson.current_build_dir(),
       should_fail : t.contains('fail'))
endforeach

python = find_program('python')
r = run_command(python, '-c', 'import h5py')
if r.returncode() == 0
  test('PythonShape', python,
    args: [files('test_shape.py'), meson.current_build_dir() / 'test_shape.h5'],
    is_parallel: false, priority: -100, timeout: 15)
endif
