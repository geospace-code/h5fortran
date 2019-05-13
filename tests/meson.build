testh5 = executable('test_hdf5', 'test_hdf5_ifc.f90',
  dependencies: hdf5,
  link_with: ooh5,
  fortran_args: quiet)
test('Test HDF5 OO', testh5, is_parallel: false)
