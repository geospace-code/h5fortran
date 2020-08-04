c = configuration_data()
is_windows = os == 'windows' ? '.true.' : '.false.'
c.set('is_windows', is_windows)
configure_file(
  input : 'pathlib.in.f90',
  output : 'pathlib.f90',
  configuration : c)

python = find_program('python')

ret = run_command(python, '-c', 'print(open("' + meson.current_source_dir() / 'reader_template.in.f90' + '").read())')
c = configuration_data()
c.set('reader_template', ret.stdout())
configure_file(
  input : 'reader.in.f90',
  output : 'reader.f90',
  configuration : c)

ret = run_command(python, '-c', 'print(open("' + meson.current_source_dir() / 'reader_lt_template.in.f90' + '").read())')
c = configuration_data()
c.set('reader_lt_template', ret.stdout())
configure_file(
  input : 'reader_lt.in.f90',
  output : 'reader_lt.f90',
  configuration : c)

ret = run_command(python, '-c', 'print(open("' + meson.current_source_dir() / 'writer_template.in.f90' + '").read())')
c = configuration_data()
c.set('writer_template', ret.stdout())
configure_file(
  input : 'writer.in.f90',
  output : 'writer.f90',
  configuration : c)

ret = run_command(python, '-c', 'print(open("' + meson.current_source_dir() / 'writer_lt_template.in.f90' + '").read())')
c = configuration_data()
c.set('writer_lt_template', ret.stdout())
configure_file(
  input : 'writer_lt.in.f90',
  output : 'writer_lt.f90',
  configuration : c)

hdf5_src = files('interface.f90',
  'read.f90', meson.current_build_dir() / 'reader.f90', meson.current_build_dir() / 'reader_lt.f90',
  'write.f90', meson.current_build_dir() /  'writer.f90', meson.current_build_dir() / 'writer_lt.f90',
  'attributes.f90',
  'string_utils.f90', meson.current_build_dir() / 'pathlib.f90')
