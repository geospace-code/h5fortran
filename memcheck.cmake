# run like:
#
#  ctest -S memcheck.cmake
#
# optionally, tell path to memory checker like:
#
# ctest -DMEMCHECK_ROOT=/path/to/bin/valgrind -S memcheck.cmake

cmake_minimum_required(VERSION 3.19)

list(APPEND opts -DCMAKE_BUILD_TYPE=Debug)

set(CTEST_TEST_TIMEOUT 60)
# takes effect only if test property TIMEOUT is not set

if(NOT DEFINED CTEST_MEMORYCHECK_TYPE)
  set(CTEST_MEMORYCHECK_TYPE "Valgrind")
endif()

if(CTEST_MEMORYCHECK_TYPE STREQUAL "Valgrind")
  # https://www.cprogramming.com/debugging/valgrind.html
  find_program(CTEST_MEMORYCHECK_COMMAND NAMES valgrind HINTS ${MEMCHECK_ROOT} REQUIRED)
  set(CTEST_MEMORYCHECK_COMMAND_OPTIONS --leak-check=full)
  set(supp ${CMAKE_CURRENT_LIST_DIR}/valgrind.supp)
  if(EXISTS ${supp})
    list(APPEND CTEST_MEMORYCHECK_COMMAND_OPTIONS --suppressions=${supp})
  endif()
elseif(CTEST_MEMORYCHECK_TYPE STREQUAL "DrMemory")
  find_program(CTEST_MEMORYCHECK_COMMAND NAMES drmemory HINTS ${MEMCHECK_ROOT} REQUIRED)
  set(CTEST_MEMORYCHECK_COMMAND_OPTIONS -light -count_leaks)
elseif(CTEST_MEMORYCHECK_TYPE STREQUAL "AddressSanitizer")
  set(check_flags -fsanitize=address)
elseif(CTEST_MEMORYCHECK_TYPE STREQUAL "LeakSanitizer")
  set(check_flags -fsanitize=leak)
elseif(CTEST_MEMORYCHECK_TYPE STREQUAL "MemorySanitizer")
  set(check_flags -fsanitize=memory)
elseif(CTEST_MEMORYCHECK_TYPE STREQUAL "ThreadSanitizer")
  set(check_flags -fsanitize=thread)
elseif(CTEST_MEMORYCHECK_TYPE STREQUAL "UndefinedBehaviorSanitizer")
  set(check_flags -fsanitize=undefined)
else()
  message(FATAL_ERROR "Unknown memory checker type: ${CTEST_MEMORYCHECK_TYPE}")
endif()

if(check_flags)
  list(APPEND opts
  -DCMAKE_C_FLAGS_DEBUG=${check_flags}
  -DCMAKE_CXX_FLAGS_DEBUG=${check_flags}
  -DCMAKE_EXE_LINKER_FLAGS_INIT=${check_flags}
  )
endif()

set(CTEST_SOURCE_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
set(CTEST_BINARY_DIRECTORY ${CTEST_SOURCE_DIRECTORY}/build-${CTEST_MEMORYCHECK_TYPE})
set(CTEST_BUILD_CONFIGURATION Debug)

if(DEFINED ENV{CMAKE_GENERATOR})
  set(CTEST_CMAKE_GENERATOR $ENV{CMAKE_GENERATOR})
elseif(WIN32)
  set(CTEST_CMAKE_GENERATOR "MinGW Makefiles")
else()
  set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
endif()

message(STATUS "Checker ${CTEST_MEMORYCHECK_TYPE}: ${CTEST_MEMORYCHECK_COMMAND}")

string(REPLACE ";" " " CTEST_MEMORYCHECK_COMMAND_OPTIONS "${CTEST_MEMORYCHECK_COMMAND_OPTIONS}")

ctest_start(Experimental)

ctest_configure(
OPTIONS "${opts}"
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
)
if(NOT (ret EQUAL 0 AND err EQUAL 0))
  message(FATAL_ERROR "CMake configure failed:  ${ret}   ${err}")
endif()

cmake_host_system_information(RESULT Ncpu QUERY NUMBER_OF_PHYSICAL_CORES)

ctest_build(
PARALLEL_LEVEL ${Ncpu}
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
)
if(NOT (ret EQUAL 0 AND err EQUAL 0))
  message(FATAL_ERROR "CMake build failed:  ${ret}   ${err}")
endif()

ctest_memcheck(
INCLUDE ${include}
INCLUDE_LABEL ${include_label}
EXCLUDE ${exclude}
EXCLUDE_LABEL ${exclude_label}
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
DEFECT_COUNT count
PARALLEL_LEVEL ${Ncpu}
)

if(NOT (ret EQUAL 0 AND err EQUAL 0))
  message(FATAL_ERROR "Memory check failed:  ${ret}   ${err}")
endif()

if(NOT count EQUAL 0)
  message(FATAL_ERROR "Memory check found ${count} defects")
endif()
