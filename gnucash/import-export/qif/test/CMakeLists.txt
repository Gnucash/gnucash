
set(QIF_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common
  ${CMAKE_SOURCE_DIR}/gnucash/import-export/qif
  ${CMAKE_SOURCE_DIR}/libgnucash/engine
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${GLIB2_INCLUDE_DIRS}
)
set(QIF_TEST_LIBS gncmod-qif test-core)

if (FALSE)
  # Tests for this directory are not run.
  gnc_add_test(test-link-qif test-link.c QIF_TEST_INCLUDE_DIRS QIF_TEST_LIBS)
  gnc_add_test(test-qif test-qif.c QIF_TEST_INCLUDE_DIRS QIF_TEST_LIBS
    GNC_TEST_FILES=${CMAKE_CURRENT_SOURCE_DIR}/test-files)
endif()

set_dist_list(test_qif_DIST CMakeLists.txt test-link.c test-qif.c test-files/test-1-bank-txn.qif)
