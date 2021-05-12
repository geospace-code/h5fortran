if(CMAKE_VERSION VERSION_LESS 3.19)
  set(zlib_url https://github.com/zlib-ng/zlib-ng/archive/refs/tags/2.0.2.zip)
  set(zlib_sha256 58a86813746ac1e2354244b1e01f22d8b4814c69e7a8fca14c657d13f67bb5bc)

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
