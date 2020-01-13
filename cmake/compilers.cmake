if(NOT PROJECT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  # this will NOT return for ExternalProject
  # this will return for FetchContent
  return()
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  if(WIN32)
    set(CMAKE_Fortran_FLAGS "/stand:f18 /traceback /warn /heap-arrays ")
  else()
    set(CMAKE_Fortran_FLAGS "-stand f18 -traceback -warn -heap-arrays ")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  string(APPEND CMAKE_Fortran_FLAGS "-march=native -fimplicit-none ")

  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    string(APPEND CMAKE_Fortran_FLAGS "-fcheck=all ")
  endif()

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8)
    string(APPEND CMAKE_Fortran_FLAGS "-std=f2018 ")
  endif()

  string(APPEND CMAKE_Fortran_FLAGS "-Wall -Wextra -Wpedantic ")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  set(CMAKE_Fortran_FLAGS "-C -Mdclchk ")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)
  set(CMAKE_Fortran_FLAGS "-W ")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  set(CMAKE_Fortran_FLAGS "-u -C=all ")
endif()
