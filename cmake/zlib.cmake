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

set(ZLIB_INCLUDE_DIRS ${CMAKE_INSTALL_FULL_INCLUDEDIR})

if(BUILD_SHARED_LIBS)
  if(WIN32)
    set(ZLIB_LIBRARIES ${CMAKE_INSTALL_FULL_BINDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}zlib1${CMAKE_SHARED_LIBRARY_SUFFIX})
  else()
    set(ZLIB_LIBRARIES ${CMAKE_INSTALL_FULL_LIBDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}z${CMAKE_SHARED_LIBRARY_SUFFIX})
  endif()
else()
  if(MSVC OR (WIN32 AND zlib_legacy))
    set(ZLIB_LIBRARIES ${CMAKE_INSTALL_FULL_LIBDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}zlibstatic${CMAKE_STATIC_LIBRARY_SUFFIX})
  else()
    set(ZLIB_LIBRARIES ${CMAKE_INSTALL_FULL_LIBDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}z${CMAKE_STATIC_LIBRARY_SUFFIX})
  endif()
endif()


set(zlib_cmake_args
-DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
-DCMAKE_BUILD_TYPE=Release
-DZLIB_COMPAT:BOOL=on
-DZLIB_ENABLE_TESTS:BOOL=off
-DZLIBNG_ENABLE_TESTS:BOOL=off
-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON
-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
)
# NetCDF 4.9/4.6 needs fPIC

if(zlib_legacy)
set(zlib_download
URL ${zlib_url}
URL_HASH SHA256=${zlib_sha256}
)
else()
set(zlib_download
GIT_REPOSITORY ${zlib_url}
GIT_TAG ${zlib_tag}
GIT_SHALLOW true
)
endif()

ExternalProject_Add(ZLIB
${zlib_download}
CMAKE_ARGS ${zlib_cmake_args}
BUILD_BYPRODUCTS ${ZLIB_LIBRARIES}
CONFIGURE_HANDLED_BY_BUILD ON
USES_TERMINAL_DOWNLOAD true
USES_TERMINAL_UPDATE true
USES_TERMINAL_PATCH true
USES_TERMINAL_CONFIGURE true
USES_TERMINAL_BUILD true
USES_TERMINAL_INSTALL true
USES_TERMINAL_TEST true
)

# --- imported target

file(MAKE_DIRECTORY ${ZLIB_INCLUDE_DIRS})
# avoid race condition

# for visibility in parent projects
add_library(ZLIB::ZLIB INTERFACE IMPORTED GLOBAL)
add_dependencies(ZLIB::ZLIB ZLIB)  # to avoid include directory race condition
target_link_libraries(ZLIB::ZLIB INTERFACE ${ZLIB_LIBRARIES})
target_include_directories(ZLIB::ZLIB INTERFACE ${ZLIB_INCLUDE_DIRS})
