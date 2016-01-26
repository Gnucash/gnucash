

SET(GENERIC_IMPORT_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${CMAKE_SOURCE_DIR}/src/import-export
  ${CMAKE_SOURCE_DIR}/src/libqof/qof
  ${CMAKE_SOURCE_DIR}/src/test-core
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)
SET(GENERIC_IMPORT_TEST_LIBS gncmod-generic-import gnc-qof test-core)

GNC_ADD_TEST_WITH_GUILE(test-import-parse test-import-parse.c
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
GNC_ADD_TEST(test-link-generic-import test-link.c
  GENERIC_IMPORT_TEST_INCLUDE_DIRS GENERIC_IMPORT_TEST_LIBS
)
