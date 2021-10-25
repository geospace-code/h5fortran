# Windows executables (Intel oneAPI, MinGW GCC, etc.) need DLL's on PATH.
# FIXME: when CMake 3.22 released, can use ENVIRONMENT_MODIFICATION

set(DLL_PATH)

if(NOT (MSVC OR BUILD_SHARED_LIBS))
  return()
endif()

if(MSVC OR (MINGW AND BUILD_SHARED_LIBS))

  if(hdf5_external)
    set(ZLIB_DLL_PATH ${ZLIB_ROOT}/bin)
  else()
    find_path(ZLIB_DLL_PATH
    NAMES zlib.dll zlib1.dll libzlib1.dll
    NO_DEFAULT_PATH
    HINTS ${ZLIB_INCLUDE_PATH}/.. ${ZLIB_ROOT} ENV ZLIB_ROOT
    PATH_SUFFIXES bin
    DOC "DLL PATH"
    )
  endif()

  if(NOT ZLIB_DLL_PATH)
    message(STATUS "Could not find DLL path, tests may fail to run.")
    return()
  endif()

  cmake_path(CONVERT "${ZLIB_DLL_PATH};$ENV{PATH}" TO_NATIVE_PATH_LIST DLL_PATH NORMALIZE)

  # this is the vital line, without it CMake set_tests_properties mangles the ENVIRONMENT
  string(REPLACE ";" "\\;" DLL_PATH "${DLL_PATH}")
elseif(UNIX AND hdf5_external)
  set(CMAKE_INSTALL_NAME_DIR ${CMAKE_INSTALL_PREFIX}/lib)
  set(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib)
endif()
