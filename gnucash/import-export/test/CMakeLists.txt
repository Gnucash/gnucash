

set(GENERIC_IMPORT_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${CMAKE_SOURCE_DIR}/gnucash/import-export
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/engine/test-core
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

set(GENERIC_IMPORT_TEST_LIBS gncmod-generic-import gncmod-test-engine test-core)

gnc_add_test_with_guile(test-import-parse test-import-parse.c
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
gnc_add_test(test-link-generic-import test-link.c
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
gnc_add_test(test-import-pending-matches test-import-pending-matches.cpp
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)

set(IMPORT_ACCOUNT_MATCHER_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/gnucash/import-export
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/libgnucash/app-utils
  ${CMAKE_SOURCE_DIR}/gnucash/gnome-utils
  ${GLIB2_INCLUDE_DIRS}
  ${GTK3_INCLUDE_DIRS}
  ${GTEST_INCLUDE_DIR}
  )

set(IMPORT_ACCOUNT_MATCHER_TEST_LIBS gncmod-generic-import gncmod-engine test-core ${GTEST_LIB})
gnc_add_test(test-import-account-matcher gtest-import-account-matcher.cpp
  IMPORT_ACCOUNT_MATCHER_TEST_INCLUDE_DIRS IMPORT_ACCOUNT_MATCHER_TEST_LIBS)

set_dist_list(test_generic_import_DIST CMakeLists.txt
  test-link.c test-import-parse.c test-import-pending-matches.cpp
  gtest-import-account-matcher.cpp)
