
message(STATUS "CMake Build Type: ${CMAKE_BUILD_TYPE}")

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)

  if(NOT WIN32)
    set(FFLAGS -stand f18 -implicitnone -traceback -warn -heap-arrays)
  else()
    set(FFLAGS /stand:f18 /4Yd /traceback /warn /heap-arrays)
    # Note: -g is /debug:full for ifort Windows
  endif()

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  list(APPEND FFLAGS -march=native -fimplicit-none)

  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    list(APPEND FFLAGS -fcheck=all)
  endif()

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8)
    list(APPEND FFLAGS -std=f2018)
  endif()

  list(APPEND FFLAGS -Wall -Wextra -Wpedantic -Werror=array-bounds -Warray-temporaries)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  list(APPEND FFLAGS -u -C=all)
endif()
