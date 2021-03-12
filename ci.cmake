cmake_minimum_required(VERSION 3.15...3.20)

set(CTEST_PROJECT_NAME "h5fortran")

set(CTEST_LABELS_FOR_SUBPROJECTS "unit;core;shaky")

# --- boilerplate follows

set(CTEST_NIGHTLY_START_TIME "01:00:00 UTC")
set(CTEST_SUBMIT_URL "https://my.cdash.org/submit.php?project=${CTEST_PROJECT_NAME}")

# --- Experimental, Nightly, Continuous
# https://cmake.org/cmake/help/latest/manual/ctest.1.html#dashboard-client-modes
if(NOT CTEST_MODEL)
  if(DEFINED ENV{CTEST_MODEL})
    set(CTEST_MODEL $ENV{CTEST_MODEL})
  endif()
endif()
if(NOT CTEST_MODEL)
  if(DEFINED ENV{CI})
    set(CI $ENV{CI})
    if(CI)
      set(CTEST_MODEL "Continuous")
    endif()
  endif()
endif()
if(NOT CTEST_MODEL)
  set(CTEST_MODEL "Experimental")
endif()

# --- other defaults
set(CTEST_TEST_TIMEOUT 10)
set(CTEST_OUTPUT_ON_FAILURE true)

set(CTEST_SOURCE_DIRECTORY ${CTEST_SCRIPT_DIRECTORY})
if(NOT DEFINED CTEST_BINARY_DIRECTORY)
  set(CTEST_BINARY_DIRECTORY ${CTEST_SOURCE_DIRECTORY}/build)
endif()

if(NOT CTEST_BUILD_CONFIGURATION)
  set(CTEST_BUILD_CONFIGURATION RelWithDebInfo)
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

find_program(GIT_EXECUTABLE NAMES git REQUIRED)

if(NOT DEFINED CTEST_BUILD_NAME)
  if(DEFINED ENV{CTEST_BUILD_NAME})
    set(CTEST_BUILD_NAME $ENV{CTEST_BUILD_NAME})
  else()
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


set(CTEST_USE_LAUNCHERS 1)

if(NOT DEFINED CTEST_CMAKE_GENERATOR)
  if(DEFINED ENV{CMAKE_GENERATOR})
    set(CTEST_CMAKE_GENERATOR $ENV{CMAKE_GENERATOR})
  else()
    include(cmake/find_generator.cmake)
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
set(CTEST_UPDATE_COMMAND git)  # avoids auto-detect version control failures on some systemes

ctest_start(${CTEST_MODEL})

if(CTEST_MODEL STREQUAL Nightly OR CTEST_MODEL STREQUAL Continuous)
  # this erases local code changes i.e. anything not "git push" already is lost forever!
  # we try to avoid that by guarding with a Git porcelain check
  execute_process(COMMAND ${GIT_EXECUTABLE} status --porcelain
    WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY}
    OUTPUT_VARIABLE _ret OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE _err)
  if(NOT _err EQUAL 0)
    message(FATAL_ERROR "CTest could not check Git porcelain status")
  endif()
  if(_ret)
    message(WARNING "CTest would have erased the non-Git Push'd changes, NOT updating.")
  else()
    ctest_update(RETURN_VALUE _ret)
    if(_ret EQUAL 0)
      message(STATUS "No Git-updated files, so no need to test. CTest stopping.")
      return()
    endif()
  endif()
endif()

ctest_configure(
  # CMAKE_BUILD_TYPE here becuase the global option seems to be ignored
  OPTIONS -DCMAKE_BUILD_TYPE=${CTEST_BUILD_CONFIGURATION}
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

ctest_test(
  # set PARALLEL_LEVEL here as the global option seems to be ignored
  PARALLEL_LEVEL ${Ncpu}
  # specify BUILD to avoid random return 255 despite tests passing
  BUILD ${CTEST_BINARY_DIRECTORY}
  SCHEDULE_RANDOM ON
  REPEAT UNTIL_PASS:3
  RETURN_VALUE _ret
  CAPTURE_CMAKE_ERROR _err
  )
ctest_submit(PARTS Test)

ctest_submit(
  PARTS Done
  BUILD_ID build_id)

if(NOT (_ret EQUAL 0 AND _err EQUAL 0))
  message(FATAL_ERROR "Build ${build_id} failed: CTest code ${_ret}, CMake code ${_err}.")
endif()

message(STATUS "OK: CTest build ${build_id}")
