
SET(QIF_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/qif
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${GLIB2_INCLUDE_DIRS}
)
SET(QIF_TEST_LIBS gncmod-qif test-core)

IF (FALSE)
  # Tests for this directory are not run.
  GNC_ADD_TEST(test-link-qif test-link.c QIF_TEST_INCLUDE_DIRS QIF_TEST_LIBS)
  GNC_ADD_TEST(test-qif test-qif.c QIF_TEST_INCLUDE_DIRS QIF_TEST_LIBS
    GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files)
ENDIF()

SET_DIST_LIST(test_qif_DIST CMakeLists.txt test-link.c test-qif.c test-files/test-1-bank-txn.qif)
