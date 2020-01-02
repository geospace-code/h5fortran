include(CheckFortranSourceCompiles)
check_fortran_source_compiles("real, dimension(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) :: x; end" f08rank15 SRC_EXT f90)

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  if(NOT WIN32)
    set(FFLAGS -stand f18 -warn declarations -traceback -warn -heap-arrays)
  else()
    set(FFLAGS /stand:f18 /warn:declarations /traceback /warn /heap-arrays)
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
  set(FFLAGS -C -Mdclchk)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)
  set(FFLAGS -W)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  list(APPEND FFLAGS -u -C=all)
endif()
