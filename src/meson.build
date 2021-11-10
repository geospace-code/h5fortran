c = configuration_data()
is_windows = os == 'windows' ? '.true.' : '.false.'
c.set('is_windows', is_windows)
configure_file(
  input : 'pathlib.in.f90',
  output : 'pathlib.f90',
  configuration : c)


subdir('read')

subdir('write')

hdf5_src = files('interface.f90',
  'attributes.f90',
  meson.current_build_dir() / 'pathlib.f90') + read_src + write_src
