include(CheckSourceCompiles)

# check C and Fortran compiler ABI compatibility

if(NOT abi_ok)
  message(CHECK_START "checking that C and Fortran compilers can link")
  try_compile(abi_ok ${CMAKE_CURRENT_BINARY_DIR}/abi_check ${CMAKE_CURRENT_LIST_DIR}/abi_check abi_check)
  if(abi_ok)
    message(CHECK_PASS "OK")
  else()
    message(FATAL_ERROR "ABI-incompatible: C compiler ${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION} and Fortran compiler ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
  endif()
endif()

# --- compiler options
# we left off "-std=f2018" type flags as they tend to issue false warnings

add_compile_options(
"$<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:-xHost;-warn;-heap-arrays>"
"$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>,$<CONFIG:Debug>>:-traceback;-check;-debug>"
"$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-mtune=native;-Wall;-fimplicit-none>"
"$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,$<CONFIG:Debug>>:-Wextra;-fcheck=all;-Werror=array-bounds>"
"$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,$<CONFIG:Release>>:-fno-backtrace;-Wno-maybe-uninitialized>"
"$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,$<CONFIG:RelWithDebInfo>>:-Wno-maybe-uninitialized>"
)

check_source_compiles(Fortran
"program test
block
end block
end program
"
f08block)
