# build Zlib to ensure compatibility.
# We use Zlib 2.x for speed and robustness.

include(ExternalProject)

# need to be sure _ROOT isn't empty, defined is not enough
if(NOT ZLIB_ROOT)
  set(ZLIB_ROOT ${CMAKE_INSTALL_PREFIX})
endif()

set(ZLIB_INCLUDE_DIR ${ZLIB_ROOT}/include)

if(BUILD_SHARED_LIBS)
  # zlib library naming on Windows is more complex than Unix-like
  set(ZLIB_LIBRARY ${ZLIB_ROOT}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}z$<$<BOOL:${WIN32}>:lib>$<IF:$<BOOL:${MSVC}>,${CMAKE_STATIC_LIBRARY_SUFFIX},${CMAKE_SHARED_LIBRARY_SUFFIX}>$<$<BOOL:${MINGW}>:.a>)
  set(ZLIB_DLL ${ZLIB_ROOT}/bin/$<$<BOOL:${WIN32}>:${CMAKE_SHARED_LIBRARY_PREFIX}zlib1.dll>)
else()
  set(zlib_mangle $<OR:$<BOOL:${MSVC}>,$<AND:$<BOOL:${zlib_legacy}>,$<BOOL:${WIN32}>>>)
  set(ZLIB_LIBRARY ${ZLIB_ROOT}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}z$<${zlib_mangle}:libstatic>${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(zlib_cmake_args
-DZLIB_COMPAT:BOOL=on
-DZLIB_ENABLE_TESTS:BOOL=off
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
-DCMAKE_BUILD_TYPE=Release
-DCMAKE_INSTALL_PREFIX:PATH=${ZLIB_ROOT}
)

ExternalProject_Add(ZLIB
URL ${zlib_url}
URL_HASH SHA256=${zlib_sha256}
CMAKE_ARGS ${zlib_cmake_args}
CMAKE_GENERATOR ${EXTPROJ_GENERATOR}
BUILD_BYPRODUCTS ${ZLIB_LIBRARY}
CONFIGURE_HANDLED_BY_BUILD ON
INACTIVITY_TIMEOUT 15
)

# --- imported target

file(MAKE_DIRECTORY ${ZLIB_INCLUDE_DIR})
# avoid race condition

add_library(ZLIB::ZLIB INTERFACE IMPORTED GLOBAL)
add_dependencies(ZLIB::ZLIB ZLIB)  # to avoid include directory race condition
target_link_libraries(ZLIB::ZLIB INTERFACE ${ZLIB_LIBRARY})
target_include_directories(ZLIB::ZLIB INTERFACE ${ZLIB_INCLUDE_DIR})

list(APPEND zlib_root -DZLIB_ROOT:PATH=${ZLIB_ROOT} -DZLIB_LIBRARY:FILEPATH=${ZLIB_LIBRARY} -DZLIB_INCLUDE_DIR:PATH=${ZLIB_INCLUDE_DIR})
