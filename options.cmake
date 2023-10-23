message(STATUS "${PROJECT_NAME} ${PROJECT_VERSION} CMake ${CMAKE_VERSION} Toolchain ${CMAKE_TOOLCHAIN_FILE}")

include(GNUInstallDirs)

option(ENABLE_COVERAGE "Code coverage tests")
option(tidy "Run clang-tidy on the code")

option(matlab "check HDF5 file writes with Matlab")
option(concepts "conceptual testing, for devs only" off)

option(CMAKE_TLS_VERIFY "Verify TLS certificates" on)

if(BUILD_SHARED_LIBS AND MSVC)
  message(WARNING "Intel oneAPI has trouble with shared libs in general on Windows, try
    cmake -DBUILD_SHARED_LIBS=off")
endif()

# Necessary for shared library with Visual Studio / Windows oneAPI
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)

file(GENERATE OUTPUT .gitignore CONTENT "*")
