# check C and Fortran compiler ABI compatibility

if(NOT abi_ok)
  message(CHECK_START "checking that C and Fortran compilers can link")
  try_compile(abi_ok
  ${CMAKE_CURRENT_BINARY_DIR}/abi_check ${CMAKE_CURRENT_LIST_DIR}/abi_check
  abi_check
  OUTPUT_VARIABLE abi_log
  )
  if(abi_ok)
    message(CHECK_PASS "OK")
  else()
    message(FATAL_ERROR "ABI-incompatible compilers:
    C compiler ${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION}
    Fortran compiler ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}
    ${abi_log}
    "
    )
  endif()
endif()

# --- compiler options
# we left off "-std=f2018" type flags as they tend to issue false warnings

if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
add_compile_options(
"$<$<COMPILE_LANGUAGE:Fortran>:-warn;-heap-arrays>"
"$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug,RelWithDebInfo>>:-traceback;-check;-debug>"
)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
add_compile_options(-Wall
$<$<COMPILE_LANGUAGE:Fortran>:-fimplicit-none>
"$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug,RelWithDebInfo>>:-Wextra;-fcheck=all;-Werror=array-bounds>"
"$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-fno-backtrace;-Wno-maybe-uninitialized>"
"$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RelWithDebInfo>>:-Wno-maybe-uninitialized>"
)
endif()

# --- code coverage
if(ENABLE_COVERAGE)
include(CodeCoverage)
append_coverage_compiler_flags()
set(COVERAGE_EXCLUDES ${PROJECT_SOURCE_DIR}/tests)
endif()
