# this must be include() before CMakeLists.txt project()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Debug or Release")
endif()

set(CMAKE_CONFIGURATION_TYPES "Release;RelWithDebInfo;Debug" CACHE STRING "Build type selections" FORCE)

# Help CMake find matching compilers, especially needed for MacOS

if(NOT DEFINED ENV{FC})
  find_program(FC NAMES gfortran)
  if(FC)
    set(ENV{FC} ${FC})
  endif()
endif()

if(DEFINED ENV{FC})
  set(FC $ENV{FC})

  if(NOT DEFINED ENV{CC})
    # use same compiler for C and Fortran, which CMake might not do itself
    if(FC MATCHES ".*ifort")
      if(WIN32)
        set(ENV{CC} icl)
      else()
        set(ENV{CC} icc)
      endif()
    elseif(FC MATCHES ".*gfortran")
      # intel compilers don't need find_program for this to work, but GCC does...
      # remember, Apple has "/usr/bin/gcc" which is really clang
      # the technique below is NECESSARY to work on Mac and not find the wrong GCC
      get_filename_component(_gcc ${FC} DIRECTORY)
      find_program(CC NAMES gcc gcc-11 gcc-10 gcc-9 gcc-8 gcc-7
        HINTS ${_gcc}
        NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH)
        # these parameters NECESSARY for Mac
      if(CC)
        set(ENV{CC} ${CC})
      endif()
    endif()
  endif()
endif()
