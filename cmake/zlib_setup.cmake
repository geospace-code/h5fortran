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
URL ${zlib_url}
URL_HASH SHA1=${zlib_sha1}
UPDATE_DISCONNECTED true
CMAKE_ARGS -DCMAKE_BUILD_TYPE=Release
BUILD_BYPRODUCTS ${_zlib_file}
INSTALL_COMMAND ""
)

# by default, Windows does not allow symbolic links, even in user directories.
# to be safe, let's just copy the zlib.h since it's a small file.
ExternalProject_Add_Step(ZLIBproj post-build
COMMAND ${CMAKE_COMMAND} -E copy ${_zlib_h} ${_zlib_build}
DEPENDEES build
BYPRODUCTS ${_zlib_build}/zlib.h)

add_library(ZLIB::ZLIB INTERFACE IMPORTED GLOBAL)
add_dependencies(ZLIB::ZLIB ZLIBproj)  # to avoid include directory race condition
target_link_libraries(ZLIB::ZLIB INTERFACE ${_zlib_file})
target_include_directories(ZLIB::ZLIB INTERFACE ${_zlib_build})

list(APPEND zlib_root -DZLIB_ROOT:PATH=${_zlib_build} -DZLIB_LIBRARY:FILEPATH=${_zlib_file} -DZLIB_INCLUDE_DIR:PATH=${_zlib_build})
