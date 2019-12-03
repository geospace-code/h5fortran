fc = meson.get_compiler('fortran')
os = host_machine.system()

quiet = fc.get_supported_arguments(['-w', '-Wno-compare-reals', '-Wno-maybe-uninitialized'])

#== find HDF5

# use_static = get_option('default_library') == 'static'
# NOTE: static: parameter is too strict for now. Works more reliably without this parameter

hdf5 = dependency('hdf5', language : 'fortran', required: false, disabler: true)  # , static: use_static
hdf5 = [hdf5]

# needed for HDF5 on some systems including Ubuntu
# these are moving to glibc so eventually this goes away
# https://github.com/mesonbuild/meson/issues/4297#issuecomment-425744841
threads = dependency('threads', required: false)
if threads.found()
  hdf5 += threads
endif
dl = fc.find_library('dl', required: false)
if dl.found()
  hdf5 += dl
endif

code = '''
use hdf5
use h5lt
implicit none
integer :: ierr

call h5open_f(ierr)
if (ierr /= 0) error stop 'could not open hdf5 library'

call h5close_f(ierr)
if (ierr /= 0) error stop 'could not close hdf5 library'

end program
'''

if not fc.links(code, dependencies: hdf5, name: 'HDF5')
  hdf5 = disabler()
endif
