# --- Help CMake find matching compilers, especially needed for MacOS

set(_paths)
if(APPLE)
  # CMAKE_SYSTEM_NAME is not set till project()
  set(_paths /usr/local/bin /opt/homebrew/bin)
  # for Homebrew that's not on PATH (can be an issue on CI)
endif()


function(find_fortran)

if(NOT DEFINED FC AND DEFINED ENV{FC})
  set(FC $ENV{FC})
endif()

if(FC)
  get_filename_component(_dir ${FC} DIRECTORY)
endif()
# determine if the user is intending to use Intel oneAPI or default Gfortran
# Need to check ifort because MKLROOT may be defined for
# use of oneMKL with Gfortran on MacOS and Linux.
if(DEFINED ENV{MKLROOT} OR FC MATCHES ".*ifort")
  find_program(FC
    NAMES ifort
    HINTS ${_dir})
endif()

find_program(FC
  NAMES gfortran gfortran-12 gfortran-11 gfortran-10 gfortran-9 gfortran-8 gfortran-7
  NAMES_PER_DIR
  HINTS ${_dir}
  PATHS ${_paths})

if(FC)
  set(ENV{FC} ${FC})
  # ENV{FC} is how project() picks up our hint
endif()

endfunction(find_fortran)


function(find_c)

if(NOT DEFINED CC)
  if(DEFINED ENV{CC})
    set(CC $ENV{CC})
  elseif(NOT DEFINED FC AND DEFINED ENV{FC})
    set(FC $ENV{FC})
  endif()
endif()

if(NOT DEFINED CC)
  # remember, Apple has "/usr/bin/gcc" which is really clang
  # the technique below is NECESSARY to work on Mac and not find the wrong GCC
  if(DEFINED FC)
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
    # get same GCC version as Gfortran
    execute_process(COMMAND ${FC} -dumpversion
      OUTPUT_VARIABLE _v
      RESULT_VARIABLE _err)
    if(NOT _err EQUAL 0)
      return()
    endif()
    string(REGEX MATCH "^([0-9]+)" _v ${_v})
    if(_v)
      set(_cc gcc-${_v})
    else()
      set(_cc gcc-12 gcc-11 gcc-10 gcc-9 gcc-8 gcc-7 gcc)  # generic last to avoid AppleClang
    endif()
  endif()
endif()

if(NOT _cc)
  return()
endif()

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
