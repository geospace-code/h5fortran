cmake_minimum_required(VERSION 3.20)

set(CTEST_PROJECT_NAME "h5fortran")

set(opts
-DCMAKE_COMPILE_WARNING_AS_ERROR:BOOL=ON
)

option(submit "use CDash upload" true)

# --- main script

set(CTEST_NIGHTLY_START_TIME "01:00:00 UTC")
set(CTEST_SUBMIT_URL "https://my.cdash.org/submit.php?project=${CTEST_PROJECT_NAME}")

if(NOT CTEST_MODEL)
  set(CTEST_MODEL "Experimental")
endif()

# --- other defaults
set(CTEST_TEST_TIMEOUT 10)

set(CTEST_USE_LAUNCHERS true)
set(CTEST_OUTPUT_ON_FAILURE true)
set(CTEST_START_WITH_EMPTY_BINARY_DIRECTORY_ONCE true)

set(CTEST_SOURCE_DIRECTORY ${CTEST_SCRIPT_DIRECTORY})
if(NOT DEFINED CTEST_BINARY_DIRECTORY)
  set(CTEST_BINARY_DIRECTORY ${CTEST_SOURCE_DIRECTORY}/build)
endif()

if(NOT DEFINED CTEST_SITE AND DEFINED ENV{CTEST_SITE})
  set(CTEST_SITE $ENV{CTEST_SITE})
endif()

find_program(GIT_EXECUTABLE NAMES git REQUIRED)

# --- CTEST_BUILD_NAME is used by ctest_submit(); must be set before ctest_start()

if(NOT CTEST_BUILD_NAME)
  execute_process(COMMAND ${GIT_EXECUTABLE} describe --tags
  WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY}
  OUTPUT_VARIABLE git_rev OUTPUT_STRIP_TRAILING_WHITESPACE
  RESULT_VARIABLE ret
  )
  if(ret EQUAL 0)
    set(CTEST_BUILD_NAME ${git_rev})
  endif()
endif()


function(find_generator)

if(NOT CTEST_CMAKE_GENERATOR AND DEFINED ENV{CMAKE_GENERATOR})
  set(CTEST_CMAKE_GENERATOR $ENV{CMAKE_GENERATOR} PARENT_SCOPE)
  return()
  # return here as if(...) wouldn't detect it in parent_scope
endif()
if(CTEST_CMAKE_GENERATOR)
  return()
endif()

find_program(ninja NAMES ninja ninja-build samu)

if(ninja)
  set(CTEST_CMAKE_GENERATOR "Ninja" PARENT_SCOPE)
elseif(WIN32)
  set(CTEST_CMAKE_GENERATOR "MinGW Makefiles" PARENT_SCOPE)
else()
  set(CTEST_CMAKE_GENERATOR "Unix Makefiles" PARENT_SCOPE)
endif()

endfunction(find_generator)

find_generator()

# --- CTest Dashboard

set(CTEST_SUBMIT_RETRY_COUNT 2)
# avoid auto-detect version control failures on some systems
set(CTEST_UPDATE_TYPE git)
set(CTEST_UPDATE_COMMAND git)

ctest_start(${CTEST_MODEL})

if(CTEST_MODEL MATCHES "(Nightly|Continuous)")
  # this erases local code changes i.e. anything not "git push" already is lost forever!
  # we try to avoid that by guarding with a Git porcelain check
  execute_process(COMMAND ${GIT_EXECUTABLE} status --porcelain
  WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY}
  TIMEOUT 5
  OUTPUT_VARIABLE ret OUTPUT_STRIP_TRAILING_WHITESPACE
  COMMAND_ERROR_IS_FATAL ANY
  )
  if(ret)
    message(FATAL_ERROR "CTest would have erased the non-Git Push'd changes.")
  else()
    ctest_update(
    RETURN_VALUE ret
    CAPTURE_CMAKE_ERROR err
    )
    if(ret LESS 0 OR NOT err EQUAL 0)
      message(FATAL_ERROR "Update failed: return ${ret} cmake return ${err}")
    endif()
    if(ret EQUAL 0 AND CTEST_MODEL STREQUAL "Continuous")
      message(NOTICE "No Git-updated files -> no need to test in CTest Model ${CTEST_MODEL}. CTest stopping.")
      return()
    endif()

  endif()
endif()

# --- configure

ctest_configure(
OPTIONS "${opts}"
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
)
if(NOT (ret EQUAL 0 AND err EQUAL 0))
  if(submit)
    ctest_submit(BUILD_ID build_id)
  endif()
  message(FATAL_ERROR "Configure ${build_id} failed: return ${ret} cmake return ${err}")
endif()

if(DEFINED ENV{CMAKE_BUILD_PARALLEL_LEVEL})
  set(Ncpu $ENV{CMAKE_BUILD_PARALLEL_LEVEL})
else()
  cmake_host_system_information(RESULT Ncpu QUERY NUMBER_OF_PHYSICAL_CORES)
endif()

ctest_build(
PARALLEL_LEVEL ${Ncpu}
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
)
if(NOT (ret EQUAL 0 AND err EQUAL 0))
  if(submit)
    ctest_submit(BUILD_ID build_id)
  endif()
  message(FATAL_ERROR "Build ${build_id} failed: return ${ret} cmake return ${err}")
endif()

if(DEFINED ENV{CTEST_PARALLEL_LEVEL})
  set(Ntest $ENV{CTEST_PARALLEL_LEVEL})
else()
  set(Ntest ${Ncpu})
endif()

ctest_test(
SCHEDULE_RANDOM true
PARALLEL_LEVEL ${Ntest}
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
)

if(submit)
  ctest_submit(BUILD_ID build_id)
endif()
if(NOT (ret EQUAL 0 AND err EQUAL 0))
  message(FATAL_ERROR "Test ${build_id} failed: CTest code ${ret}, CMake code ${err}.")
endif()

message(STATUS "OK: CTest build ${build_id}")
