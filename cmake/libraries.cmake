if(CMAKE_VERSION VERSION_LESS 3.19)
  set(zlib_url https://github.com/zlib-ng/zlib-ng/archive/refs/tags/2.0.3.zip)
  set(zlib_sha256 235232f622d17cb4070f67ebeb06b0df8cf0894f9045ea6c51782479318841d2)

  set(hdf5_url https://github.com/HDFGroup/hdf5/archive/hdf5-1_10_7.zip)
  set(hdf5_sha256 89ea0b117c27a8f7587e601b65fa1427b28db3a6ee66ffff0178509514128f18)

else()
  # CMake >= 3.19
  file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json _libj)

  if(zlib_legacy)
    string(JSON zlib_url GET ${_libj} zlib1 url)
    string(JSON zlib_sha256 GET ${_libj} zlib1 sha256)
  else()
    string(JSON zlib_url GET ${_libj} zlib2 url)
    string(JSON zlib_sha256 GET ${_libj} zlib2 sha256)
  endif()

  string(JSON hdf5_url GET ${_libj} hdf5 url)
  string(JSON hdf5_sha256 GET ${_libj} hdf5 sha256)
endif()
