conf_data = configuration_data()

is_windows = os == 'windows' ? '.true.' : '.false.'

conf_data.set('is_windows', is_windows)
pathlib_src = configure_file(
  input : 'pathlib.in.f90',
  output : 'pathlib.f90',
  configuration : conf_data)

hdf5_src = files('interface.f90',
  'read.f90', 'reader.f90', 'reader_lt.f90',
  'write.f90', 'writer.f90', 'writer_lt.f90',
  'string_utils.f90', meson.current_build_dir() / 'pathlib.f90')
