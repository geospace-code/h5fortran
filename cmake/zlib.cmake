if(NOT zlib_url)
  string(JSON zlib_url GET ${json} "zlib")
endif()

set(ZLIB_COMPAT on)
set(BUILD_TESTING off)
# set(CMAKE_POSITION_INDEPENDENT_CODE ON)
# NetCDF 4.9/4.6 needs fPIC

FetchContent_Declare(ZLIB
  URL ${zlib_url}
  FIND_PACKAGE_ARGS NAMES ZLIB
)

FetchContent_MakeAvailable(ZLIB)

if(NOT TARGET h5fortranZLIB::ZLIB)
  add_library(h5fortranZLIB::ZLIB ALIAS zlib-ng)
  set(zlib_dep DEPENDS h5fortranZLIB::ZLIB)
else()
  set(zlib_dep)
endif()
