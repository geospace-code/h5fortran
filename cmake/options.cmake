
option(hdf5_external "Build HDF5 library")
option(dev "developer mode")

option(concepts "conceptual testing, for devs only" off)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

if(NOT dev)
  set(EP_UPDATE_DISCONNECTED true)
endif()
