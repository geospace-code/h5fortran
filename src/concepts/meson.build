file_image_exe = executable('test_file_image', 'file_image.f90',
  dependencies: hdf5)
test('file_image', file_image_exe, suite:'proto', timeout: 10)
