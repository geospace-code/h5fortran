# build Zlib to ensure compatibility. This is a common practice for HDF5.

include(ExternalProject)

if(WIN32)
  set(ZLIB_name ${CMAKE_STATIC_LIBRARY_PREFIX}zlib${CMAKE_STATIC_LIBRARY_SUFFIX})
else()
  set(ZLIB_name ${CMAKE_STATIC_LIBRARY_PREFIX}z${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

# need to be sure _ROOT isn't empty, defined is not enough
if(NOT ZLIB_ROOT)
  if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(ZLIB_ROOT ${PROJECT_BINARY_DIR})
  else()
    set(ZLIB_ROOT ${CMAKE_INSTALL_PREFIX})
  endif()
endif()

set(ZLIB_INCLUDE_DIR ${ZLIB_ROOT}/include)
set(ZLIB_LIBRARY ${ZLIB_ROOT}/lib/${ZLIB_name})

ExternalProject_Add(ZLIB
URL ${zlib_url}
URL_HASH SHA256=${zlib_sha256}
CONFIGURE_HANDLED_BY_BUILD ON
INACTIVITY_TIMEOUT 30
CMAKE_ARGS -DZLIB_COMPAT:BOOL=on -DZLIB_ENABLE_TESTS:BOOL=off -DBUILD_SHARED_LIBS:BOOL=off -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX:PATH=${ZLIB_ROOT}
BUILD_BYPRODUCTS ${ZLIB_LIBRARY}
)


# --- imported target

file(MAKE_DIRECTORY ${ZLIB_INCLUDE_DIR})
# avoid race condition

add_library(ZLIB::ZLIB INTERFACE IMPORTED GLOBAL)
add_dependencies(ZLIB::ZLIB ZLIB)  # to avoid include directory race condition
target_link_libraries(ZLIB::ZLIB INTERFACE ${ZLIB_LIBRARY})
target_include_directories(ZLIB::ZLIB INTERFACE ${ZLIB_INCLUDE_DIR})

list(APPEND zlib_root -DZLIB_ROOT:PATH=${ZLIB_ROOT} -DZLIB_LIBRARY:FILEPATH=${ZLIB_LIBRARY} -DZLIB_INCLUDE_DIR:PATH=${ZLIB_INCLUDE_DIR})
