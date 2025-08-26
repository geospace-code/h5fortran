message(STATUS "${PROJECT_NAME} ${PROJECT_VERSION} CMake ${CMAKE_VERSION} Toolchain ${CMAKE_TOOLCHAIN_FILE}")

include(GNUInstallDirs)

option(h5fortran_find "try to find HDF5 library" ON)

option(h5fortran_coverage "Code coverage tests")
option(h5fortran_tidy "Run clang-tidy on the code")

option(h5fortran_matlab "check HDF5 file writes with Matlab")
option(h5fortran_python "check HDF5 file writes with Python")

option(h5fortran_IGNORE_CONDA_LIBRARIES "If ON, CMake will not search for the
hdf5 libraries in a Conda environment." ON)

option(h5fortran_BUILD_TESTING "build tests")

set_property(DIRECTORY PROPERTY EP_UPDATE_DISCONNECTED true)

# Necessary for shared library with Visual Studio / Windows oneAPI
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)

if(h5fortran_IS_TOP_LEVEL AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set_property(CACHE CMAKE_INSTALL_PREFIX PROPERTY VALUE "${PROJECT_BINARY_DIR}/local")
endif()
