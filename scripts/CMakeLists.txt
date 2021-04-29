cmake_minimum_required(VERSION 3.13...${CMAKE_VERSION})
project(HDF5_build LANGUAGES C Fortran)

include(${PROJECT_SOURCE_DIR}/../cmake/libraries.cmake)

set(EP_UPDATE_DISCONNECTED true)

message(STATUS "Building and installing HDF5 to ${CMAKE_INSTALL_PREFIX}")

include(${PROJECT_SOURCE_DIR}/../cmake/build_hdf5.cmake)
