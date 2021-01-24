
option(hdf5_external "Build HDF5 library")
option(dev "developer mode")


if(NOT dev)
  set(FETCHCONTENT_UPDATES_DISCONNECTED_ZLIB true)
  set(FETCHCONTENT_UPDATES_DISCONNECTED_HDF5 true)
endif()

# this helps linters e.g. Visual Studio Intellicode work properly
set(CMAKE_EXPORT_COMPILE_COMMANDS on)
