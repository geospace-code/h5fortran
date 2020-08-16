# --- package specific

install(FILES ${CMAKE_CURRENT_LIST_DIR}/hdf5.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod
  DESTINATION include)

# --- BOILERPLATE: install / packaging

install(TARGETS ${PROJECT_NAME}
  EXPORT ${PROJECT_NAME}Targets)

include(CMakePackageConfigHelpers)

configure_package_config_file(${CMAKE_CURRENT_LIST_DIR}/${PROJECT_NAME}Config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  INSTALL_DESTINATION lib)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  VERSION ${${PROJECT_NAME}_VERSION}
  COMPATIBILITY SameMinorVersion
)

install(EXPORT ${PROJECT_NAME}Targets
  FILE ${PROJECT_NAME}Targets.cmake
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION lib/cmake/${PROJECT_NAME}
   )

install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})

# --- CPack

if(PROJECT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  set(CPACK_GENERATOR ZIP)
  set(CPACK_PACKAGE_VENDOR "Michael Hirsch")
  set(CPACK_PACKAGE_CONTACT "Michael Hirsch")
  set(CPACK_DEBIAN_PACKAGE_DEPENDS "libhdf5-dev (>=1.10)")
  set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
  set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
  include(CPack)
endif()
