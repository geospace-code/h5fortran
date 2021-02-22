c = configuration_data()

c.set('reader_template', fs.read('reader_template.in.f90'))

configure_file(
  input : 'reader.in.f90',
  output : 'reader.f90',
  configuration : c)

# --- reader_lt

c = configuration_data()

c.set('reader_lt_template', fs.read('reader_lt_template.in.f90'))

configure_file(
  input : 'reader_lt.in.f90',
  output : 'reader_lt.f90',
  configuration : c)

read_src = files('read.f90',
  meson.current_build_dir() / 'reader.f90',
  meson.current_build_dir() / 'reader_lt.f90')
