conf_data = configuration_data()

if os == 'windows'
  code = '''
  if (scan('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ', path(1:1)) > 0 .and. &
      path(2:2) == ':') is_absolute_path = .true.
'''
else
  code = '''if(path(1:1) == '/') is_absolute_path=.true.'''
endif

conf_data.set('is_abs', code)
pathlib_src = configure_file(
  input : 'pathlib.in.f90',
  output : 'pathlib.f90',
  configuration : conf_data)

hdf5_src = files('interface.f90',
  'read.f90', 'reader.f90', 'reader_lt.f90',
  'write.f90', 'writer.f90', 'writer_lt.f90',
  'string_utils.f90', meson.current_build_dir() / 'pathlib.f90')
