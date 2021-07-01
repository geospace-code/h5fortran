# build Zlib to ensure compatibility.
# We use Zlib 2.x for speed and robustness.

include(ExternalProject)

if(MSVC OR (zlib_legacy AND WIN32))
  set(ZLIB_name ${CMAKE_STATIC_LIBRARY_PREFIX}zlibstatic${CMAKE_STATIC_LIBRARY_SUFFIX})
else()
  set(ZLIB_name ${CMAKE_STATIC_LIBRARY_PREFIX}z${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

# need to be sure _ROOT isn't empty, defined is not enough
if(NOT ZLIB_ROOT)
  if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(ZLIB_ROOT ${PROJECT_BINARY_DIR} CACHE PATH "ZLIB_ROOT")
  else()
    set(ZLIB_ROOT ${CMAKE_INSTALL_PREFIX})
  endif()
endif()

set(ZLIB_INCLUDE_DIR ${ZLIB_ROOT}/include)
set(ZLIB_LIBRARY ${ZLIB_ROOT}/lib/${ZLIB_name})

set(zlib_cmake_args
-DZLIB_COMPAT:BOOL=on
-DZLIB_ENABLE_TESTS:BOOL=off
-DBUILD_SHARED_LIBS:BOOL=off
-DCMAKE_BUILD_TYPE=Release
-DCMAKE_INSTALL_PREFIX:PATH=${ZLIB_ROOT})

if(zlib_git)
ExternalProject_Add(ZLIB
GIT_REPOSITORY ${zlib_git}
GIT_TAG ${zlib_tag}
CONFIGURE_HANDLED_BY_BUILD ON
INACTIVITY_TIMEOUT 15
CMAKE_ARGS ${zlib_cmake_args}
BUILD_BYPRODUCTS ${ZLIB_LIBRARY}
)
else()
ExternalProject_Add(ZLIB
URL ${zlib_url}
URL_HASH SHA256=${zlib_sha256}
CONFIGURE_HANDLED_BY_BUILD ON
INACTIVITY_TIMEOUT 15
CMAKE_ARGS ${zlib_cmake_args}
BUILD_BYPRODUCTS ${ZLIB_LIBRARY}
)
endif(zlib_git)

# --- imported target

file(MAKE_DIRECTORY ${ZLIB_INCLUDE_DIR})
# avoid race condition

add_library(ZLIB::ZLIB INTERFACE IMPORTED GLOBAL)
add_dependencies(ZLIB::ZLIB ZLIB)  # to avoid include directory race condition
target_link_libraries(ZLIB::ZLIB INTERFACE ${ZLIB_LIBRARY})
target_include_directories(ZLIB::ZLIB INTERFACE ${ZLIB_INCLUDE_DIR})

list(APPEND zlib_root -DZLIB_ROOT:PATH=${ZLIB_ROOT} -DZLIB_LIBRARY:FILEPATH=${ZLIB_LIBRARY} -DZLIB_INCLUDE_DIR:PATH=${ZLIB_INCLUDE_DIR})
