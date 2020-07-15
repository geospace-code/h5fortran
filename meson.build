project('h5fortran', 'fortran',
  meson_version : '>=0.52.0',
  version : '2.10.3',
  default_options : ['default_library=static', 'buildtype=release', 'warning_level=3'])

subdir('meson')

subdir('src')

ooh5 = library('h5fortran',
  sources: hdf5_src,
  dependencies: hdf5,
  install: true)

hdf5_interface = declare_dependency(link_with: ooh5, dependencies: hdf5)

# --- testing
if not meson.is_subproject()
  subdir('src/tests')
endif
