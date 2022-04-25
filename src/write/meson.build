configure_file(
  input : 'writer.in.f90',
  output : 'writer.f90',
  configuration : {
    'writer_template': fs.read('writer_template.f90')
  })

# --- writer_lt

c = configuration_data()

c.set('writer_lt_template', fs.read('writer_lt_template.in.f90'))

configure_file(
  input : 'writer_lt.in.f90',
  output : 'writer_lt.f90',
  configuration : c)

write_src = files('write.f90', 'scalar.f90',
  meson.current_build_dir() /  'writer.f90',
  meson.current_build_dir() / 'writer_lt.f90')
