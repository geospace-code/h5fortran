# this must be include() before CMakeLists.txt project()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Debug or Release")
endif()

set(CMAKE_CONFIGURATION_TYPES "Release;RelWithDebInfo;Debug" CACHE STRING "Build type selections" FORCE)

# --- Help CMake find matching compilers, especially needed for MacOS

set(_paths)
if(APPLE)
  # CMAKE_SYSTEM_NAME is not set till project()
  set(_paths /usr/local/bin /opt/homebrew/bin)
  # for Homebrew that's not on PATH (can be an issue on CI)
endif()


function(find_fortran)

set(_fc)
if(DEFINED FC)
  set(_fc ${FC})
elseif(DEFINED ENV{FC})
  set(_fc $ENV{FC})
endif()

if(_fc)
  get_filename_component(_dir ${_fc} DIRECTORY)
endif()
# determine if the user is intending to use Intel oneAPI or default Gfortran
# Need to check ifort because MKLROOT may be defined for
# use of oneMKL with Gfortran on MacOS and Linux.
if(DEFINED ENV{MKLROOT} OR _fc MATCHES ".*ifort")
  find_program(FC
    NAMES ifort
    PATHS ${_dir})
endif()

find_program(FC
  NAMES gfortran gfortran-12 gfortran-11 gfortran-10 gfortran-9 gfortran-8 gfortran-7
  NAMES_PER_DIR
  PATHS ${_dir} ${_paths})

if(FC)
  set(ENV{FC} ${FC})
  # ENV{FC} is how project() picks up our hint
endif()

endfunction(find_fortran)


function(find_c)

set(_cc)
if(DEFINED CC)
  set(_cc ${CC})
elseif(DEFINED ENV{CC})
  set(_cc $ENV{CC})
endif()

if(NOT _cc)
  # remember, Apple has "/usr/bin/gcc" which is really clang
  # the technique below is NECESSARY to work on Mac and not find the wrong GCC
  if(FC)
    get_filename_component(_dir ${FC} DIRECTORY)
  endif()
  # use same compiler for C and Fortran, which CMake might not do itself
  if(FC MATCHES ".*ifort")
    if(WIN32)
      set(_cc icl)
    else()
      set(_cc icc)
    endif()
  elseif(FC MATCHES ".*gfortran")
    set(_cc gcc-12 gcc-11 gcc-10 gcc-9 gcc-8 gcc-7 gcc)  # generic last to avoid AppleClang
  endif()
endif()

if(NOT _cc)
  return()
endif()

# FIXME: search for gcc- with same suffix as gfortran-

find_program(CC
  NAMES ${_cc}
  NAMES_PER_DIR
  PATHS ${_paths}  # PATHS are searched last
  HINTS ${_dir}
  NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH)

if(CC)
  set(ENV{CC} ${CC})
  # ENV{CC} is how project() picks up our hint
endif()

endfunction(find_c)


function(find_c_fortran)
find_fortran()
find_c()
endfunction(find_c_fortran)
