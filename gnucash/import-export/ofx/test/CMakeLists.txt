

if(WITH_OFX)
  set(OFX_TEST_INCLUDE_DIRS ${LIBOFX_INCLUDE_DIRS})
  set(OFX_TEST_LIBS)
  set(OFX_RESPONSE_TEST_INCLUDE_DIRS
    ${LIBOFX_INCLUDE_DIRS}
    ${CMAKE_BINARY_DIR}/common
    ${CMAKE_SOURCE_DIR}/common/test-core
    ${CMAKE_SOURCE_DIR}/gnucash/import-export
    ${CMAKE_SOURCE_DIR}/gnucash/gnome
    ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/core-utils
    ${CMAKE_SOURCE_DIR}/libgnucash/engine
    ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
    ${GUILE_INCLUDE_DIRS}
    ${GTEST_INCLUDE_DIR}
  )
  set(OFX_RESPONSE_TEST_LIBS
    gnc-generic-import gnc-engine gnc-app-utils gnc-core-utils
    gnc-gnome-utils gnc-gnome gnc-module gnc-test-engine test-core
    gtest
    ${LIBOFX_LDFLAGS}
  )

  gnc_add_test(test-link-ofx test-link.c OFX_TEST_INCLUDE_DIRS OFX_TEST_LIBS)
  gnc_add_test_with_guile(gtest-ofx-import-response gtest-ofx-import-response.cpp
    OFX_RESPONSE_TEST_INCLUDE_DIRS OFX_RESPONSE_TEST_LIBS)
  set_tests_properties(gtest-ofx-import-response PROPERTIES TIMEOUT 30)
  set_property(TEST gtest-ofx-import-response APPEND PROPERTY ENVIRONMENT
    "GSETTINGS_SCHEMA_DIR=${SCHEMADIR_BUILD}" "GSETTINGS_BACKEND=memory")
  if(COMPILE_GSCHEMAS)
    add_dependencies(gtest-ofx-import-response compiled-schemas)
  endif()
endif()

set_dist_list(test_ofx_DIST CMakeLists.txt test-link.c gtest-ofx-import-response.cpp)
