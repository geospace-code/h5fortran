test_minimal = executable('test_minimal', 'test_minimal.f90', dependencies: h5fortran)

test('minimal', test_minimal,
args: meson.current_build_dir(),
suite: 'unit',
priority: 100,
timeout: 10
)

python = find_program('python')
h5py_ok = run_command(python, '-c', 'import h5py', check: false).returncode() == 0
if h5py_ok
  test_string_py = meson.current_build_dir() / 'test_string_py.h5'

  test('PythonAttributes', python,
  args: [files('test_attributes.py'), meson.current_build_dir() / 'test_attr.h5'],
  is_parallel: true, priority: 50, timeout: 15
  )

  test('PythonShape', python,
  args: [files('test_shape.py'), meson.current_build_dir() / 'test_shape.h5'],
  is_parallel: false, priority: -100, timeout: 15
  )

  test('PythonString', python,
  args: [files('test_string.py'), test_string_py],
  is_parallel: true, priority: 50, timeout: 15
  )
endif

foreach t : ['array', 'attributes', 'cast',
'deflate_write', 'deflate_read', 'deflate_props',
'destructor', 'exist', 'fill', 'groups', 'layout', 'lt',
'scalar', 'shape', 'string', 'version', 'write',
'fail_read_size_mismatch', 'fail_read_rank_mismatch', 'fail_nonexist_variable']

  e = executable('test_'+t, 'test_' + t + '.f90', dependencies: h5fortran)
  if ['shape', 'deflate_write', 'write'].contains(t)
    par = true
    pri = 50
  elif ['deflate_read', 'deflate_props', 'string_read'].contains(t)
    par = false
    pri = -100
  else
    par = true
    pri = 0
  endif
  test(t, e, suite: 'unit', timeout: 10, is_parallel: par, priority: pri,
       workdir : meson.current_build_dir(),
       should_fail : t.contains('fail'))
endforeach
