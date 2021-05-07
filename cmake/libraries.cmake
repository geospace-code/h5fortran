if(CMAKE_VERSION VERSION_LESS 3.19)
  set(zlib_url https://zlib.net/zlib1211.zip)
  set(zlib_sha256 d7510a8ee1918b7d0cad197a089c0a2cd4d6df05fee22389f67f115e738b178d)

  set(hdf5_url https://github.com/HDFGroup/hdf5/archive/hdf5-1_10_7.zip)
  set(hdf5_sha256 89ea0b117c27a8f7587e601b65fa1427b28db3a6ee66ffff0178509514128f18)

else()
  # CMake >= 3.19
  file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json _libj)

  string(JSON zlib_url GET ${_libj} zlib url)
  string(JSON zlib_sha256 GET ${_libj} zlib sha256)

  string(JSON hdf5_url GET ${_libj} hdf5 url)
  string(JSON hdf5_sha256 GET ${_libj} hdf5 sha256)
endif()
