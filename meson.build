project('h5fortran', 'fortran',
meson_version : '>=0.57.0'
)

fc = meson.get_compiler('fortran')
os = host_machine.system()

#== find HDF5

# use_static = get_option('default_library') == 'static'
# NOTE: static: parameter is too strict for now. Works more reliably without this parameter

hdf5 = dependency('hdf5', language : 'fortran', required: false, disabler: true)  # , static: use_static

if not fc.links(files('test/test_minimal.f90'), dependencies: hdf5, name: 'HDF5')
  hdf5 = disabler()
endif

# spills test file into source directory. Need to PR Meson to allow working dir spec or env:
# if (fc.run(files('test/test_minimal.f90'), dependencies: hdf5, name: 'HDF5').returncode() != 0)
#  hdf5 = disabler()
# endif

if not meson.is_subproject()
  assert(hdf5.found(), 'hdf5 library is required for h5fortran')
endif

subdir('src')

ooh5 = library('h5fortran',
sources: hdf5_src,
dependencies: hdf5,
install: true)

h5fortran = declare_dependency(link_with: ooh5, dependencies: hdf5)

# --- testing
if not meson.is_subproject()
  subdir('test')
endif
