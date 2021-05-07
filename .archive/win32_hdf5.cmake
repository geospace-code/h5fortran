# NOTE: this isn't used anymore. I may delete in the future. I think it's better to just build your own HDF5 rather than use the shaky download for Windows.

function(win32_hdf5_env)
# Windows with HDF5 needs this interesting workaround of
# copying HDF5 dlls to the CMAKE_BINARY_DIR.
# adding the dll path to PATH should have worked, but didn't.
#
# This workaround relies on having either:
# * 'cmake -DHDF5_ROOT=' variable
# * environment variable HDF5_ROOT

if(NOT DEFINED HDF5_ROOT)
  if(DEFINED ENV{HDF5_ROOT})
    file(TO_CMAKE_PATH "$ENV{HDF5_ROOT}" HDF5_ROOT)
  else()
    message(STATUS "HDF5 workaround for Windows Intel compiler not applied because HDF5_ROOT environment variable not set. It would be something like C:/Program Files/HDF_Group/HDF5/1.12.0")
    return()
  endif()
endif()

# bizarre behavior if not strip quotes
# string(REPLACE "\"" "" HDF5_ROOT ${HDF5_ROOT})
# This "should" have worked, but the following workaround is needed instead.
# set_tests_properties(${TESTNAME} PROPERTIES ENVIRONMENT "PATH=${HDF5_ROOT}/bin;$ENV{PATH}")

foreach(_f hdf5.dll hdf5_f90cstub.dll hdf5_fortran.dll hdf5_hl.dll hdf5_hl_f90cstub.dll hdf5_hl_fortran.dll)
if(NOT EXISTS ${PROJECT_BINARY_DIR}/${_f})
  message(VERBOSE " ${HDF5_ROOT}/bin/${_f} => ${PROJECT_BINARY_DIR}")
  file(COPY ${HDF5_ROOT}/bin/${_f} DESTINATION ${PROJECT_BINARY_DIR})
endif()
endforeach()

endfunction(win32_hdf5_env)
