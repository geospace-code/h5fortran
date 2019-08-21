quiet = ['-w']

fc = meson.get_compiler('fortran')
f18flag = fc.first_supported_argument(['-std=f2018', '-stand f18', '/stand:f18'])
if fc.get_id() == 'gcc'
  add_project_arguments('-fimplicit-none', f18flag, language : 'fortran')
  quiet = ['-Wno-compare-reals', '-Wno-maybe-uninitialized']
elif fcid == 'intel'
  add_project_arguments('-warn', '-heap-arrays', f18flag, language : 'fortran')
elif fcid == 'intel-cl'
  add_project_arguments('/warn', '/heap-arrays', f18flag, language : 'fortran')
elif fcid == 'pgi'
  add_project_arguments('-C', '-Mdclchk', language : 'fortran')
endif


hdf5 = dependency('hdf5', language : 'fortran', required: false)
h5incdir = []
if not hdf5.found()
  h5libdir = get_option('h5libdir')
  if h5libdir == ''
    h5libdir = []
  endif
  h5incdir = include_directories(get_option('h5incdir'))
  hdf5_libs = []
  foreach name : ['hdf5', 'hdf5_fortran', 'hdf5_hl', 'hdf5_hl_fortran', # msys2 names
                  'hdf5_serial', 'hdf5_serial_fortran',
                  'hdf5_serial_hl', 'hdf5_serialhl_fortran']
    lib = fc.find_library(name, required: false, dirs: h5libdir)
    if lib.found()
      hdf5_libs += lib
    endif
  endforeach
  if hdf5_libs.length() == 0
    error('could not find HDF5 library')
  endif
  # hdf5 = declare_dependency(dependencies: hdf5_libs, include_directories: h5incdir)  # meson bug?
  hdf5 = hdf5_libs
endif
h5run = fc.run('use h5lt; end', dependencies: hdf5, include_directories: h5incdir, name: 'HDF5 runs')
if h5run.returncode() != 0
  warning('HDF5 possible linking problems: ' +  h5run.stdout() + h5run.stderr())
endif