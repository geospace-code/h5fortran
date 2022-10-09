# run like:
#
#  ctest -S memcheck.cmake
#
# optionally, tell path to memory checker like:
#
# ctest -DMEMCHECK_ROOT=/path/to/bin/valgrind -S memcheck.cmake

list(APPEND opts -DCMAKE_BUILD_TYPE=Debug)

set(CTEST_TEST_TIMEOUT 60)
# takes effect only if test property TIMEOUT is not set

if(NOT DEFINED CTEST_MEMORYCHECK_TYPE)
  set(CTEST_MEMORYCHECK_TYPE "Valgrind")
endif()

if(CTEST_MEMORYCHECK_TYPE STREQUAL "Valgrind")
  # https://www.cprogramming.com/debugging/valgrind.html
  find_program(exe NAMES valgrind HINTS ${MEMCHECK_ROOT} PATH_SUFFIXES bin REQUIRED)
  set(CTEST_MEMORYCHECK_COMMAND ${exe})
elseif(CTEST_MEMORYCHECK_TYPE STREQUAL "DrMemory")
  find_program(exe NAMES drmemory HINTS ${MEMCHECK_ROOT} PATH_SUFFIXES bin64 bin REQUIRED)
  set(CTEST_MEMORYCHECK_COMMAND ${exe})
  set(CTEST_MEMORYCHECK_COMMAND_OPTIONS "-light -count_leaks")
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

if(WIN32)
  set(CTEST_CMAKE_GENERATOR "MinGW Makefiles")
else()
  set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
endif()
set(CTEST_BUILD_FLAGS -j)

message(STATUS "Checker ${CTEST_MEMORYCHECK_TYPE}: ${CTEST_MEMORYCHECK_COMMAND}")

ctest_start(Experimental)

ctest_configure(
OPTIONS "${opts}"
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
)
if(NOT (ret EQUAL 0 AND err EQUAL 0))
  message(FATAL_ERROR "CMake configure failed:  ${ret}   ${err}")
endif()

ctest_build(
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
)
if(NOT (ret EQUAL 0 AND err EQUAL 0))
  message(FATAL_ERROR "CMake build failed:  ${ret}   ${err}")
endif()

ctest_memcheck(
RETURN_VALUE ret
CAPTURE_CMAKE_ERROR err
DEFECT_COUNT count
)

if(NOT (ret EQUAL 0 AND err EQUAL 0))
  message(FATAL_ERROR "Memory check failed:  ${ret}   ${err}")
endif()

if(NOT count EQUAL 0)
  message(FATAL_ERROR "Memory check found ${count} defects")
endif()
