# run by:
# ctest -S setup.cmake

# --- Project-specific -Doptions
# these will be used if the project isn't already configured.
set(_opts)

# --- boilerplate follows
message(STATUS "CMake ${CMAKE_VERSION}")
if(CMAKE_VERSION VERSION_LESS 3.15)
  message(FATAL_ERROR "Please update CMake >= 3.15")
endif()

# site is OS name
if(NOT DEFINED CTEST_SITE)
  set(CTEST_SITE ${CMAKE_SYSTEM_NAME})
endif()

# parallel test--use ctest_test(PARALLEL_LEVEL ${Ncpu} as setting CTEST_PARALLEL_LEVEL has no effect
include(ProcessorCount)
ProcessorCount(Ncpu)
message(STATUS "${Ncpu} CPU cores detected")

# test name is Fortran compiler in FC
# Note: ctest scripts cannot read cache variables like CMAKE_Fortran_COMPILER
if(NOT DEFINED ENV{FC})
  find_program(FC NAMES gfortran)
  if(FC)
    set(ENV{FC} ${FC})
  endif()
endif()

if(DEFINED ENV{FC})
  set(FC $ENV{FC})
  set(CTEST_BUILD_NAME ${FC})

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

if(NOT DEFINED CTEST_BUILD_CONFIGURATION)
  set(CTEST_BUILD_CONFIGURATION "Release")
endif()

set(CTEST_SOURCE_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
if(NOT DEFINED CTEST_BINARY_DIRECTORY)
  set(CTEST_BINARY_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/build)
endif()

# CTEST_CMAKE_GENERATOR must be defined in any case here.
if(NOT DEFINED CTEST_CMAKE_GENERATOR AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.17)
  find_program(_gen NAMES ninja ninja-build samu)
  if(_gen)
    execute_process(COMMAND ${_gen} --version
      OUTPUT_VARIABLE _ninja_version
      OUTPUT_STRIP_TRAILING_WHITESPACE
      RESULT_VARIABLE _gen_ok
      TIMEOUT 10)
    if(_gen_ok EQUAL 0 AND _ninja_version VERSION_GREATER_EQUAL 1.10)
      set(CTEST_CMAKE_GENERATOR "Ninja")
    endif()
  endif(_gen)
endif()
if(NOT DEFINED CTEST_CMAKE_GENERATOR)
  if(WIN32)
    set(CTEST_CMAKE_GENERATOR "MinGW Makefiles")
    set(CTEST_BUILD_FLAGS -j)  # not --parallel as this goes to generator directly
  else()
    set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
    set(CTEST_BUILD_FLAGS -j)  # not --parallel as this goes to generator directly
  endif()
endif()

# -- build and test
ctest_start("Experimental" ${CTEST_SOURCE_DIRECTORY} ${CTEST_BINARY_DIRECTORY})

ctest_configure(
  BUILD ${CTEST_BINARY_DIRECTORY}
  SOURCE ${CTEST_SOURCE_DIRECTORY}
  OPTIONS "${_opts}"
  RETURN_VALUE return_code
  CAPTURE_CMAKE_ERROR cmake_err)

# if it's a generator or compiler mismatch, delete cache and try again
if(NOT cmake_err EQUAL 0)
  file(REMOVE ${CTEST_BINARY_DIRECTORY}/CMakeCache.txt)

  ctest_configure(
    BUILD ${CTEST_BINARY_DIRECTORY}
    SOURCE ${CTEST_SOURCE_DIRECTORY}
    OPTIONS "${_opts}"
    RETURN_VALUE return_code
    CAPTURE_CMAKE_ERROR cmake_err)
endif()

if(return_code EQUAL 0 AND cmake_err EQUAL 0)
  ctest_build(
    BUILD ${CTEST_BINARY_DIRECTORY}
    CONFIGURATION ${CTEST_BUILD_CONFIGURATION}
    RETURN_VALUE return_code
    NUMBER_ERRORS Nerror
    CAPTURE_CMAKE_ERROR cmake_err
    )
else()
  message(STATUS "SKIP: ctest_build(): returncode: ${return_code}; CMake error code: ${cmake_err}")
endif()

if(return_code EQUAL 0 AND Nerror EQUAL 0 AND cmake_err EQUAL 0)
  ctest_test(
  BUILD ${CTEST_BINARY_DIRECTORY}
  RETURN_VALUE return_code
  CAPTURE_CMAKE_ERROR ctest_err
  PARALLEL_LEVEL ${Ncpu}
  )
else()
  message(STATUS "SKIP: ctest_test(): returncode: ${return_code}; CMake error code: ${cmake_err}")
endif()

ctest_submit()

if(NOT (return_code EQUAL 0 AND Nerror EQUAL 0 AND cmake_err EQUAL 0 AND ctest_err EQUAL 0))
  message(FATAL_ERROR "Build and test failed.")
endif()
