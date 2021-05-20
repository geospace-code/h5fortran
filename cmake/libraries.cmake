if(CMAKE_VERSION VERSION_LESS 3.19)
  set(zlib_git https://github.com/zlib-ng/zlib-ng.git)
  set(zlib_tag bde11b12775aed5113040de319f484da879a3f04)

  set(hdf5_url https://github.com/HDFGroup/hdf5/archive/hdf5-1_10_7.zip)
  set(hdf5_sha256 89ea0b117c27a8f7587e601b65fa1427b28db3a6ee66ffff0178509514128f18)

else()
  # CMake >= 3.19
  file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json _libj)

  if(zlib_legacy)
    string(JSON zlib_url GET ${_libj} zlib1 url)
    string(JSON zlib_sha256 GET ${_libj} zlib1 sha256)
  else()
    string(JSON zlib_git GET ${_libj} zlib2 git)
    string(JSON zlib_tag GET ${_libj} zlib2 tag)
  endif()

  string(JSON hdf5_url GET ${_libj} hdf5 url)
  string(JSON hdf5_sha256 GET ${_libj} hdf5 sha256)
endif()
