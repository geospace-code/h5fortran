# build Zlib to ensure compatibility.
# We use Zlib 2.x for speed and robustness.
include(GNUInstallDirs)
include(ExternalProject)

if(zlib_legacy)
  string(JSON zlib_url GET ${json} zlib1 url)
  string(JSON zlib_sha256 GET ${json} zlib1 sha256)
else()
  string(JSON zlib_url GET ${json} zlib2 url)
  string(JSON zlib_tag GET ${json} zlib2 tag)
endif()

set(zlib_cmake_args
-DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
-DCMAKE_BUILD_TYPE=Release
-DZLIB_COMPAT:BOOL=on
-DZLIB_ENABLE_TESTS:BOOL=off
-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON
-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
)
# NetCDF 4.9/4.6 needs fPIC

if(zlib_legacy)
ExternalProject_Add(ZLIB
URL ${zlib_url}
URL_HASH SHA256=${zlib_sha256}
CMAKE_ARGS ${zlib_cmake_args}
CONFIGURE_HANDLED_BY_BUILD ON
INACTIVITY_TIMEOUT 60
)
else()
ExternalProject_Add(ZLIB
GIT_REPOSITORY ${zlib_url}
GIT_TAG ${zlib_tag}
GIT_SHALLOW true
CMAKE_ARGS ${zlib_cmake_args}
CONFIGURE_HANDLED_BY_BUILD ON
INACTIVITY_TIMEOUT 60
)
endif()
