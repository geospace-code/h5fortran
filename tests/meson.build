testh5 = executable('test_hdf5', 'test_hdf5_ifc.f90',
  dependencies: hdf5_interface,
  fortran_args: quiet)
test('h5interface', testh5,
  is_parallel: false,
  suite: 'h5fortran',
  timeout: 15)
