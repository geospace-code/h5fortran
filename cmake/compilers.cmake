include(CheckFortranCompilerFlag)
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

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  if(WIN32)
    # add_compile_options(/Qdiag-error-limit:3)
    string(APPEND CMAKE_Fortran_FLAGS " /warn /heap-arrays")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " /traceback /check:bounds /debug:all")
  else()
    # add_compile_options(-diag-error-limit=3)
    string(APPEND CMAKE_Fortran_FLAGS " -warn -heap-arrays")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -traceback -check all -debug extended")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  string(APPEND CMAKE_Fortran_FLAGS " -mtune=native -Wall -fimplicit-none")
  string(APPEND CMAKE_Fortran_FLAGS_RELEASE " -fno-backtrace -Wno-maybe-uninitialized")
  string(APPEND CMAKE_Fortran_FLAGS_RELWITHDEBINFO " -Wno-maybe-uninitialized")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -Wextra -fcheck=all -Werror=array-bounds")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  string(APPEND CMAKE_Fortran_FLAGS " -f2018 -u -C=all")
endif()

check_source_compiles(Fortran
"program test
block
end block
end program
"
f08block)
