if(CMAKE_VERSION VERSION_LESS 3.19)
  set(zlib_url https://zlib.net/zlib1211.zip)
  set(zlib_sha1 bccd93ad3cee39c3d08eee68d45b3e11910299f2)

  set(hdf5_url https://github.com/HDFGroup/hdf5/archive/hdf5-1_10_7.zip)
  set(hdf5_sha1 caae5aa9d5ef2a2c3f4c05830a8ccf4d0ac9ce88)

else()
  # CMake >= 3.19
  file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json _libj)

  string(JSON zlib_url GET ${_libj} zlib url)
  string(JSON zlib_sha1 GET ${_libj} zlib sha1)

  string(JSON hdf5_url GET ${_libj} hdf5 url)
  string(JSON hdf5_sha1 GET ${_libj} hdf5 sha1)
endif()
