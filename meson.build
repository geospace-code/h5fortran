project('Object-oriented HDF5 Fortran', 'fortran',
  meson_version : '>=0.51.2',
  version : '1.0.0')

quiet = ['-w']

fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
  add_project_arguments('-O', '-fimplicit-none', '-Wextra', '-Wpedantic', language : 'fortran')
  quiet = ['-Wno-compare-reals', '-Wno-maybe-uninitialized']
  if fc.version().version_compare('>=8')
    add_project_arguments('-std=f2018', language : 'fortran')
  endif
elif fcid == 'intel'
  add_project_arguments('-warn', '-heap-arrays', '-stand f18', language : 'fortran')
elif fcid == 'intel-cl'
  add_project_arguments('/warn', '/heap-arrays', '/stand:f18', language : 'fortran')
elif fcid == 'pgi'
  add_project_arguments('-C', '-Mdclchk', language : 'fortran')
endif

hdf5 = dependency('hdf5', language : 'fortran')

h5run = fc.run('use h5lt; end', dependencies: hdf5, name: 'HDF5')
if h5run.returncode() != 0
  error('HDF5 linking problems: ' + h5run.stderr())
endif

subdir('src')
ooh5 = library('oohdf5',
  sources: hdf5_src,
  dependencies: hdf5
)

# --- testing
subdir('tests')

shapes = executable('shapes_hdf5', 'tests/GetShape.f90',
  dependencies: hdf5,
  link_with: ooh5,
  fortran_args: quiet)
test('Shapes', shapes, args: ['p5.h5','group69/flux_node'], is_parallel: false)
