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
