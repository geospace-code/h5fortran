# even factory FindMPI missing this on say MacOS despite printing MPI_VERSION on find
function(check_mpi_version)

if(MPI_VERSION)
  return()
endif()

message(CHECK_START "Checking MPI API level")

if(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/find_mpi/get_mpi_version.c)
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/find_mpi/get_mpi_version.c
  [=[
  #include <mpi.h>
  #include <stdio.h>

  int main(void) {
  int version, subversion;

  int ierr = MPI_Get_version(&version, &subversion);
  if (ierr != 0) return 1;
  printf("CMAKE_MPI_VERSION %d.%d\n", version, subversion);

  return 0;
  }
  ]=]
  )
endif()

try_run(mpi_run_code mpi_build_code
${CMAKE_CURRENT_BINARY_DIR}/find_mpi/build
${CMAKE_CURRENT_BINARY_DIR}/find_mpi/get_mpi_version.c
CMAKE_FLAGS "-DINCLUDE_DIRECTORIES:PATH=${MPI_C_INCLUDE_DIRS}"
# must have quotes as include_dirs is a list
# LINK_OPTIONS ${MPI_C_LINK_FLAGS}  # breaks CentOS GCC with -Wl,-rpath
LINK_LIBRARIES ${MPI_C_LIBRARIES}
RUN_OUTPUT_VARIABLE MPI_VERSION_STRING
COMPILE_OUTPUT_VARIABLE mpi_vers_build_out
)

if(NOT mpi_build_code)
  message(CHECK_FAIL "MPI_VERSION test failed to build:
  ${mpi_vers_build_out}"
  )
  return()
endif()

# We don't if(mpi_run_code EQUAL 0) as some platforms / MPI libs don't precisely
# return 0. The regex should be enough.
if("${MPI_VERSION_STRING}" MATCHES "CMAKE_MPI_VERSION ([0-9]+\\.[0-9]+)")
  set(MPI_VERSION ${CMAKE_MATCH_1} CACHE STRING "MPI API level")
  message(CHECK_PASS "${MPI_VERSION}")
endif()

endfunction(check_mpi_version)
