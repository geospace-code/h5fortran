quiet = ['-w']

fc = meson.get_compiler('fortran')
os = target_machine.system()

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

#== find HDF5

# needed for HDF5 on some systems including Ubuntu
threads = dependency('threads', required: false)
dl = fc.find_library('dl', required: false)

use_static = get_option('default_library') == 'static'
# NOTE: static: parameter is too strict for now. Works more reliably without this parameter

hdf5 = dependency('hdf5', language : 'fortran', required: false, disabler: true)  # , static: use_static
if fc.links('use h5lt; end', dependencies: hdf5, name: 'HDF5')
  h5incdir = []
  hdf5 = [hdf5, threads, dl]
  subdir_done()
endif

# HDF5 was not found
h5incdir = include_directories(get_option('h5incdir'))
h5libdir = get_option('h5libdir')
if h5libdir == ''
  if os == 'darwin'
    # assume homebrew
    h5libdir = '/usr/local/opt/hdf5/lib'
    h5incdir = include_directories('/usr/local/opt/hdf5/include')
  elif os == 'windows'
    # assume MSYS2
    h5libdir = 'c:/msys64/mingw64/lib'
    if use_static
      h5incdir = include_directories('c:/msys64/mingw64/include/static')
    else
      h5incdir = include_directories('c:/msys64/mingw64/include/shared')
    endif
  else
    h5libdir = []
  endif
endif

hdf5_libs = []
foreach name : ['hdf5', 'hdf5_fortran', 'hdf5_hl', 'hdf5_hl_fortran', # msys2 names
                'hdf5_serial', 'hdf5_serial_fortran',
                'hdf5_serial_hl', 'hdf5_serialhl_fortran']
  lib = fc.find_library(name, required: false, dirs: h5libdir)  # , static: use_static
  if lib.found()
    hdf5_libs += lib
  endif
endforeach


if not fc.links('use h5lt; end', dependencies: hdf5_libs, include_directories: h5incdir, name: 'HDF5')
  hdf5 = disabler()
else
  hdf5 = declare_dependency(dependencies: [hdf5_libs, threads, dl], include_directories: h5incdir)
endif
