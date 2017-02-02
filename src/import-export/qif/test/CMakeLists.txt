
SET(QIF_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src
  ${CMAKE_SOURCE_DIR}/src/import-export/qif
  ${CMAKE_SOURCE_DIR}/src/libqof/qof
  ${CMAKE_SOURCE_DIR}/src/test-core
  ${GLIB2_INCLUDE_DIRS}
)
SET(QIF_TEST_LIBS gncmod-qif test-core)

IF (FALSE)
  # Tests for this directory are not run.
  GNC_ADD_TEST(test-link-qif test-link.c QIF_TEST_INCLUDE_DIRS QIF_TEST_LIBS)
  GNC_ADD_TEST(test-qif test-qif.c QIF_TEST_INCLUDE_DIRS QIF_TEST_LIBS
    GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files)
ENDIF()

SET_DIST_LIST(test_qif_DIST CMakeLists.txt Makefile.am test-link.c test-qif.c test-files/test-1-bank-txn.qif)
