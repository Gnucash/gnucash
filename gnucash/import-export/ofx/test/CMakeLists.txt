

if(WITH_OFX)
  set(OFX_TEST_INCLUDE_DIRS ${LIBOFX_INCLUDE_DIRS})
  set(OFX_TEST_LIBS)

  gnc_add_test(test-link-ofx test-link.c OFX_TEST_INCLUDE_DIRS OFX_TEST_LIBS)

  set(OFX_LIFECYCLE_TEST_INCLUDE_DIRS
    ${CMAKE_BINARY_DIR}/common
    ${CMAKE_SOURCE_DIR}/gnucash/import-export
    ${CMAKE_SOURCE_DIR}/gnucash/import-export/ofx
    ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${GTEST_INCLUDE_DIR})
  add_library(gnc-ofx-import-lifecycle-test-source STATIC
    ../gnc-ofx-import.cpp
    ../gnc-ofx-import-teardown.cpp)
  set_target_properties(gnc-ofx-import-lifecycle-test-source
    PROPERTIES EXCLUDE_FROM_ALL TRUE)
  set_source_files_properties(
    ../gnc-ofx-import.cpp
    ../gnc-ofx-import-teardown.cpp
    PROPERTIES OBJECT_DEPENDS ${CONFIG_H})
  target_compile_definitions(gnc-ofx-import-lifecycle-test-source PRIVATE
    GNC_OFX_IMPORT_TEST_SEAM
    -DG_LOG_DOMAIN="gnc.import.ofx.test")
  target_include_directories(gnc-ofx-import-lifecycle-test-source PRIVATE
    ${OFX_LIFECYCLE_TEST_INCLUDE_DIRS}
    ${LIBOFX_INCLUDE_DIRS})
  target_link_libraries(gnc-ofx-import-lifecycle-test-source PUBLIC
    gnc-generic-import gnc-engine gnc-app-utils gnc-core-utils
    gnc-gnome-utils gnc-gnome gnc-module
    ${LIBOFX_LDFLAGS} ${OFX_LIBSTDCXX})
  set(OFX_LIFECYCLE_TEST_LIBS
    gnc-ofx-import-lifecycle-test-source
    test-core
    gtest)
  gnc_add_test(test-ofx-import-lifecycle gtest-ofx-import-lifecycle.cpp
    OFX_LIFECYCLE_TEST_INCLUDE_DIRS OFX_LIFECYCLE_TEST_LIBS)
endif()

set_dist_list(test_ofx_DIST
  CMakeLists.txt
  gtest-ofx-import-lifecycle.cpp
  test-link.c)
