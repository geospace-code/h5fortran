option(ENABLE_COVERAGE "Code coverage tests")

option(matlab "check HDF5 file writes with Matlab")
option(concepts "conceptual testing, for devs only" off)

message(STATUS "${PROJECT_NAME} ${PROJECT_VERSION}  CMake ${CMAKE_VERSION}")

set(CMAKE_TLS_VERIFY true)


if(BUILD_SHARED_LIBS)
  if(WIN32 AND CMAKE_VERSION VERSION_LESS 3.21 AND ${PROJECT_NAME}_BUILD_TESTING)
    message(STATUS "Windows with shared libs needs CMake >= 3.21 to run tests")
  endif()
  if(MSVC)
    message(WARNING "Intel oneAPI has trouble with shared libs in general on Windows, try
      cmake -DBUILD_SHARED_LIBS=off")
  endif()
endif()

cmake_path(SET CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/Modules)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  # will not take effect without FORCE
  # CMAKE_BINARY_DIR in case it's used from FetchContent
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR} CACHE PATH "Install top-level directory" FORCE)
endif()

# Rpath options necessary for shared library install to work correctly in user projects
set(CMAKE_INSTALL_NAME_DIR ${CMAKE_INSTALL_PREFIX}/lib)
set(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/lib)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH true)

# Necessary for shared library with Visual Studio / Windows oneAPI
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()
