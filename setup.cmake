cmake_minimum_required(VERSION 3.14...3.20)

set(CTEST_PROJECT_NAME "h5fortran")
set(CTEST_NIGHTLY_START_TIME "01:00:00 UTC")
set(CTEST_MODEL "Experimental")
set(CTEST_SUBMIT_URL "https://my.cdash.org/submit.php?project=${CTEST_PROJECT_NAME}")

set(CTEST_LABELS_FOR_SUBPROJECTS "unit;shaky")

# --- boilerplate follows
set(CTEST_TEST_TIMEOUT 10)
set(CTEST_OUTPUT_ON_FAILURE true)

set(CTEST_SOURCE_DIRECTORY ${CTEST_SCRIPT_DIRECTORY})
if(NOT DEFINED CTEST_BINARY_DIRECTORY)
  set(CTEST_BINARY_DIRECTORY ${CTEST_SOURCE_DIRECTORY}/build)
endif()

if(NOT DEFINED CTEST_BUILD_CONFIGURATION)
  set(CTEST_BUILD_CONFIGURATION "Release")
endif()

if(NOT DEFINED CTEST_SITE)
  if(DEFINED ENV{CTEST_SITE})
    set(CTEST_SITE $ENV{CTEST_SITE})
  else()
    cmake_host_system_information(RESULT sys_name QUERY OS_NAME OS_RELEASE OS_VERSION)
    string(REPLACE ";" " " sys_name ${sys_name})
    set(CTEST_SITE ${sys_name})
  endif()
endif()

if(NOT DEFINED CTEST_BUILD_NAME)
  if(DEFINED ENV{CTEST_BUILD_NAME})
    set(CTEST_BUILD_NAME $ENV{CTEST_BUILD_NAME})
  else()
    find_program(GIT_EXECUTABLE NAMES git REQUIRED)
    execute_process(COMMAND ${GIT_EXECUTABLE} describe --tags
      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY}
      OUTPUT_VARIABLE git_rev OUTPUT_STRIP_TRAILING_WHITESPACE
      RESULT_VARIABLE _err)
    if(NOT _err EQUAL 0)
      # old Git
      execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
        WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY}
        OUTPUT_VARIABLE git_rev OUTPUT_STRIP_TRAILING_WHITESPACE
        RESULT_VARIABLE _err)
    endif()
    if(_err EQUAL 0)
      set(CTEST_BUILD_NAME ${git_rev})
    endif()
  endif()
endif()

# CTEST_CMAKE_GENERATOR must always be defined
if(NOT DEFINED CTEST_CMAKE_GENERATOR AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.17)
  find_program(ninja NAMES ninja ninja-build samu)
  if(ninja)
    execute_process(COMMAND ${ninja} --version
      OUTPUT_VARIABLE ninja_version
      OUTPUT_STRIP_TRAILING_WHITESPACE
      RESULT_VARIABLE err
      TIMEOUT 10)
    if(err EQUAL 0 AND ninja_version VERSION_GREATER_EQUAL 1.10)
      set(CTEST_CMAKE_GENERATOR Ninja)
    endif()
  endif(ninja)
endif()
if(NOT DEFINED CTEST_CMAKE_GENERATOR)
  set(CTEST_BUILD_FLAGS -j)  # not --parallel as this goes to generator directly
  if(WIN32)
    set(CTEST_CMAKE_GENERATOR "MinGW Makefiles")
  else()
    set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
  endif()
endif()

# --- test parallelism is used for setup and plotting
include(ProcessorCount)

function(cmake_cpu_count)
  # on ARM e.g. Raspberry Pi, the usually reliable cmake_host_system_info gives 1 instead of true count
  # fallback to less reliable ProcessorCount which does work on Raspberry Pi.
  ProcessorCount(_ncount)
  cmake_host_system_information(RESULT Ncpu QUERY NUMBER_OF_PHYSICAL_CORES)

  if(Ncpu EQUAL 1 AND _ncount GREATER 0)
    set(Ncpu ${_ncount})
  endif()

  set(Ncpu ${Ncpu} PARENT_SCOPE)

endfunction(cmake_cpu_count)
cmake_cpu_count()

# --- CTest Dashboard

set(CTEST_NOTES_FILES "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")
set(CTEST_SUBMIT_RETRY_COUNT 3)

ctest_start(${CTEST_MODEL})
# ctest_submit(PARTS Notes)

ctest_configure(
  RETURN_VALUE _ret
  CAPTURE_CMAKE_ERROR _err)
ctest_submit(PARTS Configure)
if(NOT (_ret EQUAL 0 AND _err EQUAL 0))
  message(FATAL_ERROR "Configure failed.")
endif()

ctest_build(
  RETURN_VALUE _ret
  CAPTURE_CMAKE_ERROR _err)
ctest_submit(PARTS Build)
if(NOT (_ret EQUAL 0 AND _err EQUAL 0))
  message(FATAL_ERROR "Build failed.")
endif()

ctest_test(PARALLEL_LEVEL ${Ncpu})
ctest_submit(PARTS Test)

ctest_submit(PARTS Done)
