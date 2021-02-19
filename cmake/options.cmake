
option(hdf5_external "Build HDF5 library")
option(dev "developer mode")

option(concepts "conceptual testing, for devs only" off)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

if(NOT dev)
  set(FETCHCONTENT_UPDATES_DISCONNECTED_ZLIB true)
  set(FETCHCONTENT_UPDATES_DISCONNECTED_HDF5 true)
endif()
