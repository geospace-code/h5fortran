
option(hdf5_external "Build HDF5 library")
option(dev "developer mode")

option(shaky "run shaky tests that may segfault since designed to fail" off)
option(matlab "check HDF5 file writes with Matlab" off)
option(concepts "conceptual testing, for devs only" off)

set(CMAKE_EXPORT_COMPILE_COMMANDS true)

set(CMAKE_TLS_VERIFY true)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "top-level install path" FORCE)
endif()

if(dev)
else()
  set_directory_properties(PROPERTIES EP_UPDATE_DISCONNECTED true)
endif()

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()
