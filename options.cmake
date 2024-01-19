message(STATUS "${PROJECT_NAME} ${PROJECT_VERSION} CMake ${CMAKE_VERSION} Toolchain ${CMAKE_TOOLCHAIN_FILE}")

include(GNUInstallDirs)

option(find "try to find libraries" on)

option(ENABLE_COVERAGE "Code coverage tests")
option(tidy "Run clang-tidy on the code")

option(matlab "check HDF5 file writes with Matlab")
option(concepts "conceptual testing, for devs only" off)

option(CMAKE_TLS_VERIFY "Verify TLS certificates" on)

if(BUILD_SHARED_LIBS AND MSVC)
  message(WARNING "Intel oneAPI has trouble with shared libs in general on Windows, try
    cmake -DBUILD_SHARED_LIBS=off")
endif()

option(h5fortran_BUILD_TESTING "build tests" ${PROJECT_IS_TOP_LEVEL})

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT AND PROJECT_IS_TOP_LEVEL)
  set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/local" CACHE PATH "path to install" FORCE)
endif()

set_property(DIRECTORY PROPERTY EP_UPDATE_DISCONNECTED true)

# Necessary for shared library with Visual Studio / Windows oneAPI
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)
