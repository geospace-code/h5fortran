option(autobuild "auto-build HDF5 if missing/broken" on)

option(ENABLE_COVERAGE "Code coverage tests")
option(dev "developer mode")

option(matlab "check HDF5 file writes with Matlab" off)
option(concepts "conceptual testing, for devs only" off)

option(zlib_legacy "use old ZLIB 1.x")

set(CMAKE_EXPORT_COMPILE_COMMANDS true)

set(CMAKE_TLS_VERIFY true)

if(dev)
else()
  set_directory_properties(PROPERTIES EP_UPDATE_DISCONNECTED true)
endif()

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  # will not take effect without FORCE
  # CMAKE_BINARY_DIR in case it's used from FetchContent
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR} CACHE PATH "Install top-level directory" FORCE)
endif()

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()
