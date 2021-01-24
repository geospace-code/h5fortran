include(FeatureSummary)

set_package_properties(ZLIB PROPERTIES
TYPE RECOMMENDED
URL "https://www.zlib.net/"
DESCRIPTION "compression library"
PURPOSE "HDF5 lossless compression")

set_package_properties(Python3 PROPERTIES
TYPE OPTIONAL
URL "https://python.org"
DESCRIPTION "interpreter"
PURPOSE "(optional) Python used for testing of h5fortran")

add_feature_info(BuildHDF5 hdf5_external "build HDF5 library")


if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
  feature_summary(WHAT ENABLED_FEATURES PACKAGES_FOUND)
else()
  feature_summary(WHAT ENABLED_FEATURES)
endif()
