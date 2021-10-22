# Windows executables (Intel oneAPI, MinGW GCC, etc.) need DLL's on PATH.
# NOTE: CMake 3.22 added test property ENVIRONMENT_MODIFICATION that may do this more smoothly:
# https://cmake.org/cmake/help/git-stage/prop_test/ENVIRONMENT_MODIFICATION.html

set(dll_path)

if(NOT (MSVC OR (MINGW AND BUILD_SHARED_LIBS)))
  return()
endif()

if(hdf5_external)
  set(ZLIB_DLL_DIR ${ZLIB_ROOT}/bin)
else()
  find_path(ZLIB_DLL_DIR
  NAMES zlib.dll zlib1.dll libzlib1.dll
  NO_DEFAULT_PATH
  HINTS ${ZLIB_INCLUDE_DIR}/.. ${ZLIB_ROOT} ENV ZLIB_ROOT
  PATH_SUFFIXES bin
  DOC "DLL PATH"
  )
endif()

if(NOT ZLIB_DLL_DIR)
  message(STATUS "Could not find DLL path, tests may fail.")
  return()
endif()

set(dll_path "${ZLIB_DLL_DIR};$ENV{PATH}")
cmake_path(CONVERT "${dll_path}" TO_NATIVE_PATH_LIST dll_path NORMALIZE)

# this is the vital line, without it CMake set_tests_properties mangles the ENVIRONMENT
string(REPLACE ";" "\\;" dll_path "${dll_path}")
