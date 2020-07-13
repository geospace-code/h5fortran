hdf5_src = files('interface.f90',
  'read.f90', 'reader.f90', 'reader_lt.f90',
  'write.f90', 'writer.f90', 'writer_lt.f90',
  'string_utils.f90')

if get_option('prototype')
  file_image_exe = executable('test_file_image', 'file_image.f90',
    dependencies: hdf5)
  test('file_image', file_image_exe, suite:'proto', timeout: 10)
endif
