# build Zlib to ensure compatibility. This is a common practice for HDF5.

if(WIN32)
  set(_zlib_name ${CMAKE_STATIC_LIBRARY_PREFIX}zlibstatic${CMAKE_STATIC_LIBRARY_SUFFIX})
else()
  set(_zlib_name ${CMAKE_STATIC_LIBRARY_PREFIX}z${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(_zlib_file ${PROJECT_BINARY_DIR}/ZLIBproj-prefix/src/ZLIBproj-build/${_zlib_name})
set(_zlib_build ${PROJECT_BINARY_DIR}/ZLIBproj-prefix/src/ZLIBproj-build)
set(_zlib_h ${PROJECT_BINARY_DIR}/ZLIBproj-prefix/src/ZLIBproj/zlib.h)

ExternalProject_Add(ZLIBproj
URL https://zlib.net/zlib1211.zip
URL_HASH SHA1=bccd93ad3cee39c3d08eee68d45b3e11910299f2
UPDATE_DISCONNECTED true
CMAKE_ARGS -DCMAKE_BUILD_TYPE=Release
BUILD_BYPRODUCTS ${_zlib_file}
INSTALL_COMMAND ""
)

if(NOT IS_DIRECTORY ${_zlib_build})
  file(MAKE_DIRECTORY ${_zlib_build})  # avoid race condition
endif()
if(NOT EXISTS ${_zlib_build}/zlib.h)
  # by default, Windows does not allow symbolic links, even in user directories.
  # so to be safe, let's just copy the zlib.h on Windows since it's a small file.
  if(WIN32)
    file(COPY ${_zlib_h} DESTINATION ${_zlib_build})
  else(WIN32)
    file(CREATE_LINK ${_zlib_h} ${_zlib_build}/zlib.h SYMBOLIC)
  endif(WIN32)
endif()

add_library(ZLIB::ZLIB INTERFACE IMPORTED GLOBAL)
target_link_libraries(ZLIB::ZLIB INTERFACE ${_zlib_file})
target_include_directories(ZLIB::ZLIB INTERFACE ${_zlib_build})

list(APPEND zlib_root -DZLIB_ROOT:PATH=${_zlib_build} -DZLIB_LIBRARY:FILEPATH=${_zlib_file} -DZLIB_INCLUDE_DIR:PATH=${_zlib_build})
