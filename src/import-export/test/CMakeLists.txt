

SET(GENERIC_IMPORT_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${CMAKE_SOURCE_DIR}/src/import-export
  ${CMAKE_SOURCE_DIR}/src/libqof/qof
  ${CMAKE_SOURCE_DIR}/src/test-core
  ${CMAKE_SOURCE_DIR}/src/engine/test-core
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

SET(GENERIC_IMPORT_TEST_LIBS gncmod-generic-import gncmod-test-engine gnc-qof test-core)

GNC_ADD_TEST_WITH_GUILE(test-import-parse test-import-parse.c
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
GNC_ADD_TEST(test-link-generic-import test-link.c
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
GNC_ADD_TEST(test-flat-bayes test-flat-bayes.c
    GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
GNC_ADD_TEST(test-import-pending-matches test-import-pending-matches.c
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
SET_DIST_LIST(test_generic_import_DIST CMakeLists.txt Makefile.am
        test-link.c test-import-parse.c test-import-pending-matches.c
        test-flat-bayes.c)
