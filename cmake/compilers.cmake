include(CheckFortranCompilerFlag)

# check C-Fortran ABI compatibility
try_compile(abi_ok ${CMAKE_CURRENT_BINARY_DIR}/abi_check ${CMAKE_CURRENT_LIST_DIR}/abi_check abi_check)
if(abi_ok)
  message(STATUS "C and Fortran compiler detected to be ABI-compatible.")
else()
  message(FATAL ERROR "C compiler {CMAKE_C_COMPILER_ID} {CMAKE_C_COMPILER_VERSION} and Fortran compiler ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION} are ABI-incompatible.")
endif()

# ----

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  if(WIN32)
    add_compile_options(/Qdiag-error-limit:3)
    string(APPEND CMAKE_Fortran_FLAGS " /stand:f18 /traceback /warn /heap-arrays")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " /check:bounds /debug:all")
  else()
    add_compile_options(-diag-error-limit=3)
    string(APPEND CMAKE_Fortran_FLAGS " -stand f18 -traceback -warn -heap-arrays")
    string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -check all -debug extended")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-mtune=native -Wall -Wextra -fmax-errors=3)
  string(APPEND CMAKE_Fortran_FLAGS " -fimplicit-none")
  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fcheck=all -Werror=array-bounds")

  check_fortran_compiler_flag(-std=f2018 f2018flag)
  if(f2018flag)
    string(APPEND CMAKE_Fortran_FLAGS " -std=f2018")
  endif()

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_EQUAL 9.3.0)
    # makes a lot of spurious warnings on allocatable scalar character
    string(APPEND CMAKE_Fortran_FLAGS " -Wno-maybe-uninitialized")
  elseif(CMAKE_Fortran_COMPILER_VERSION VERSION_EQUAL 10.2.0)
    # avoid spurious warning on intrinsic :: rank
    string(APPEND CMAKE_Fortran_FLAGS " -Wno-surprising")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  string(APPEND CMAKE_Fortran_FLAGS " -f2018 -u -C=all")
endif()

include(CheckFortranSourceCompiles)
check_fortran_source_compiles("implicit none (type, external); end" f2018impnone SRC_EXT f90)
if(NOT f2018impnone)
  message(FATAL_ERROR "Compiler does not support Fortran 2018 implicit none (type, external): ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
endif()
