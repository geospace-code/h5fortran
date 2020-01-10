fc = meson.get_compiler('fortran')
os = host_machine.system()

quiet = fc.get_supported_arguments(['-w', '-Wno-compare-reals', '-Wno-maybe-uninitialized'])

# -- stub for future > 7-D
# f08rank15 = fc.links('real, dimension(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) :: x; end', name: 'f2008 rank 15')

#== find HDF5

# use_static = get_option('default_library') == 'static'
# NOTE: static: parameter is too strict for now. Works more reliably without this parameter

hdf5 = dependency('hdf5', language : 'fortran', required: false, disabler: true)  # , static: use_static

if not fc.links(files('../src/tests/test_minimal.f90'), dependencies: hdf5, name: 'HDF5')
  hdf5 = disabler()
endif

if not meson.is_subproject()
  assert(hdf5.found(), 'hdf5 library is required for h5fortran')
endif